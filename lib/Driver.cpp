#include "mull/Driver.h"

#include "mull/Config/Configuration.h"
#include "mull/Diagnostics/Diagnostics.h"
#include "mull/Filters/Filters.h"
#include "mull/Filters/FunctionFilter.h"
#include "mull/MutationResult.h"
#include "mull/MutationsFinder.h"
#include "mull/Parallelization/Parallelization.h"
#include "mull/Program/Program.h"
#include "mull/ReachableFunction.h"
#include "mull/Result.h"
#include "mull/Toolchain/Runner.h"

#include <llvm/ProfileData/Coverage/CoverageMapping.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/Path.h>

#include <algorithm>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace llvm;
using namespace llvm::object;
using namespace mull;
using namespace std;

Driver::~Driver() {
  delete this->ideDiagnostics;
}

std::unique_ptr<Result> Driver::run() {
  auto mutationPoints = findMutationPoints();
  auto filteredMutations = filterMutations(std::move(mutationPoints));
  auto mutationResults = runMutations(filteredMutations);

  return std::make_unique<Result>(std::move(mutationResults), std::move(filteredMutations));
}

std::vector<MutationPoint *> Driver::findMutationPoints() {
  if (!config.skipSanityCheckRun) {
    Runner runner(diagnostics);
    singleTask.execute("Sanity check run", [&]() {
      ExecutionResult result =
          runner.runProgram(config.executable, {}, {}, config.timeout, config.captureTestOutput);
      if (result.status != Passed) {
        std::stringstream failureMessage;
        failureMessage << "Original test failed\n";
        failureMessage << "test: ";
        failureMessage << "main"
                       << "\n";
        failureMessage << "status: ";
        failureMessage << result.getStatusAsString() << "\n";
        failureMessage << "stdout: '";
        failureMessage << result.stdoutOutput << "'\n";
        failureMessage << "stderr: '";
        failureMessage << result.stderrOutput << "'\n";
        diagnostics.warning(failureMessage.str());
      }
    });
  }

  std::vector<FunctionUnderTest> functionsUnderTest = getFunctionsUnderTest();
  std::vector<FunctionUnderTest> filteredFunctions = filterFunctions(functionsUnderTest);

  selectInstructions(filteredFunctions);

  std::vector<MutationPoint *> mutationPoints =
      mutationsFinder.getMutationPoints(diagnostics, program, filteredFunctions);

  return mutationPoints;
}

std::vector<MutationPoint *> Driver::filterMutations(std::vector<MutationPoint *> mutationPoints) {
  std::vector<MutationPoint *> mutations = std::move(mutationPoints);

  for (auto filter : filters.mutationFilters) {
    std::vector<MutationPoint *> tmp;
    parallelRun(diagnostics,
                "Applying filter: "s + filter->name(),
                mutations,
                tmp,
                config.parallelization.workers,
                [&](MutationPoint *point) -> std::optional<MutationPoint *> {
                  if (!filter->shouldSkip(point)) {
                    return point;
                  }
                  return std::nullopt;
                });
    mutations = std::move(tmp);
  }

  return mutations;
}

std::vector<FunctionUnderTest> Driver::filterFunctions(std::vector<FunctionUnderTest> functions) {
  std::vector<FunctionUnderTest> filteredFunctions(std::move(functions));

  for (auto filter : filters.functionFilters) {
    std::vector<FunctionUnderTest> tmp;
    parallelRun(diagnostics,
                "Applying function filter: "s + filter->name(),
                filteredFunctions,
                tmp,
                config.parallelization.workers,
                [&](FunctionUnderTest &fut) -> std::optional<FunctionUnderTest> {
                  if (!filter->shouldSkip(fut.getFunction())) {
                    return fut;
                  }
                  return std::nullopt;
                });
    filteredFunctions = std::move(tmp);
  }

  return filteredFunctions;
}

void Driver::selectInstructions(std::vector<FunctionUnderTest> &functions) {
  std::vector<int> Nothing;
  parallelRun(diagnostics,
              "Instruction selection",
              functions,
              Nothing,
              config.parallelization.workers,
              [&](FunctionUnderTest &fut) {
                fut.selectInstructions(filters.instructionFilters);
                return std::nullopt;
              });
}

std::vector<std::unique_ptr<MutationResult>>
Driver::runMutations(std::vector<MutationPoint *> &mutationPoints) {
  if (mutationPoints.empty()) {
    return std::vector<std::unique_ptr<MutationResult>>();
  }

  if (config.dryRunEnabled) {
    return dryRunMutations(mutationPoints);
  }

  return normalRunMutations(mutationPoints);
}

#pragma mark -

std::vector<std::unique_ptr<MutationResult>>
Driver::dryRunMutations(const std::vector<MutationPoint *> &mutationPoints) {
  std::vector<std::unique_ptr<MutationResult>> mutationResults;
  parallelRun(diagnostics,
              "Running mutants (dry run)",
              mutationPoints,
              mutationResults,
              config.parallelization.workers,
              [&](const MutationPoint *point) {
                ExecutionResult result;
                result.status = DryRun;
                return std::make_unique<MutationResult>(result, point);
              });
  return mutationResults;
}

std::vector<std::unique_ptr<MutationResult>>
Driver::normalRunMutations(const std::vector<MutationPoint *> &mutationPoints) {
  singleTask.execute("Prepare mutations", [&]() {
    for (auto point : mutationPoints) {
      point->getBitcode()->addMutation(point);
    }
  });

  auto workers = config.parallelization.workers;
  std::vector<int> devNull;

  parallelRun(diagnostics,
              "Cloning functions for mutation",
              program.bitcode(),
              devNull,
              workers,
              [&](std::unique_ptr<Bitcode> &bitcode) {
                CloneMutatedFunctionsTask::cloneFunctions(*bitcode);
                return std::nullopt;
              });
  parallelRun(diagnostics,
              "Removing original functions",
              program.bitcode(),
              devNull,
              workers,
              [&](std::unique_ptr<Bitcode> &bitcode) {
                DeleteOriginalFunctionsTask::deleteFunctions(*bitcode);
                return std::nullopt;
              });
  parallelRun(diagnostics,
              "Redirect mutated functions",
              program.bitcode(),
              devNull,
              workers,
              [&](std::unique_ptr<Bitcode> &bitcode) {
                InsertMutationTrampolinesTask::insertTrampolines(*bitcode);
                return std::nullopt;
              });
  parallelRun(diagnostics,
              "Applying mutations",
              mutationPoints,
              devNull,
              workers,
              [&](const MutationPoint *point) {
                point->applyMutation();
                return std::nullopt;
              });

  std::vector<std::string> objectFiles;
  parallelRun(diagnostics,
              "Compiling original code",
              program.bitcode(),
              objectFiles,
              workers,
              [&](std::unique_ptr<Bitcode> &bitcode) {
                return toolchain.compiler().compileBitcode(*bitcode);
              });

  std::string executable;
  singleTask.execute("Link mutated program",
                     [&]() { executable = toolchain.linker().linkObjectFiles(objectFiles); });

  Runner runner(diagnostics);
  /// On macOS, sometimes newly compiled programs take more time to execute for the first run
  /// As we take the execution time as a baseline for timeout it makes sense to have an additional
  /// warm up run so that the next runs will be a bit faster
  singleTask.execute("Warm up run", [&]() {
    runner.runProgram(executable, {}, {}, config.timeout, config.captureMutantOutput);
  });

  ExecutionResult baseline;
  singleTask.execute("Baseline run", [&]() {
    baseline = runner.runProgram(executable, {}, {}, config.timeout, config.captureMutantOutput);
  });

  std::vector<std::unique_ptr<MutationResult>> mutationResults;
  parallelRun(diagnostics,
              "Running mutants",
              mutationPoints,
              mutationResults,
              config.parallelization.mutantExecutionWorkers,
              [&](const MutationPoint *mutationPoint) {
                ExecutionResult result = runner.runProgram(executable,
                                                           {},
                                                           { mutationPoint->getUserIdentifier() },
                                                           baseline.runningTime * 10,
                                                           config.captureMutantOutput);

                return std::make_unique<MutationResult>(result, mutationPoint);
              });

  return mutationResults;
}

Driver::Driver(Diagnostics &diagnostics, const Configuration &config, Program &program,
               Toolchain &t, Filters &filters, MutationsFinder &mutationsFinder)
    : config(config), program(program), toolchain(t), mutationsFinder(mutationsFinder),
      diagnostics(diagnostics), filters(filters), singleTask(diagnostics) {

  if (config.diagnostics != IDEDiagnosticsKind::None) {
    this->ideDiagnostics = new NormalIDEDiagnostics(config.diagnostics);
  } else {
    this->ideDiagnostics = new NullIDEDiagnostics();
  }
}

static std::unique_ptr<llvm::coverage::CoverageMapping>
loadCoverage(const Configuration &configuration, Diagnostics &diagnostics) {
  if (configuration.coverageInfo.empty()) {
    return nullptr;
  }
  llvm::Expected<std::unique_ptr<llvm::coverage::CoverageMapping>> maybeMapping =
      llvm::coverage::CoverageMapping::load({ configuration.executable },
                                            configuration.coverageInfo);
  if (!maybeMapping) {
    std::string error;
    llvm::raw_string_ostream os(error);
    llvm::logAllUnhandledErrors(maybeMapping.takeError(), os, "Cannot read coverage info: ");
    diagnostics.warning(os.str());
    return nullptr;
  }
  return std::move(maybeMapping.get());
}

std::vector<FunctionUnderTest> Driver::getFunctionsUnderTest() {
  std::vector<FunctionUnderTest> functionsUnderTest;

  singleTask.execute("Gathering functions under test", [&]() {
    std::unique_ptr<llvm::coverage::CoverageMapping> coverage = loadCoverage(config, diagnostics);
    if (coverage) {
      /// Some of the function records contain just name, the others are prefixed with the filename
      /// to avoid collisions
      /// TODO: check case when filename:functionName is not enough to resolve collisions
      /// TODO: pick a proper data structure
      std::unordered_map<std::string, std::unordered_set<std::string>> scopedFunctions;
      std::unordered_set<std::string> unscopedFunctions;
      for (auto &it : coverage->getCoveredFunctions()) {
        if (!it.ExecutionCount) {
          continue;
        }
        std::string scope;
        std::string name = it.Name;
        size_t idx = name.find(':');
        if (idx != std::string::npos) {
          scope = name.substr(0, idx);
          name = name.substr(idx + 1);
        }
        if (scope.empty()) {
          unscopedFunctions.insert(name);
        } else {
          scopedFunctions[scope].insert(name);
        }
      }
      for (auto &bitcode : program.bitcode()) {
        for (llvm::Function &function : *bitcode->getModule()) {
          bool covered = false;
          std::string name = function.getName().str();
          if (unscopedFunctions.count(name)) {
            covered = true;
          } else {
            std::string filepath = SourceLocation::locationFromFunction(&function).unitFilePath;
            std::string scope = llvm::sys::path::filename(filepath).str();
            if (scopedFunctions[scope].count(name)) {
              covered = true;
            }
          }
          if (covered) {
            functionsUnderTest.emplace_back(&function, bitcode.get());
          }
        }
      }
    } else {
      for (auto &bitcode : program.bitcode()) {
        for (llvm::Function &function : *bitcode->getModule()) {
          functionsUnderTest.emplace_back(&function, bitcode.get());
        }
      }
    }
  });

  return functionsUnderTest;
}
