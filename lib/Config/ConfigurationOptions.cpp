#include <utility>

#include <utility>

#include <utility>

#include "mull/Config/ConfigurationOptions.h"

#include <sys/types.h>
#include <thread>

namespace mull {

std::string diagnosticsToString(Diagnostics diagnostics) {
  switch (diagnostics) {
  case Diagnostics::None: {
    return "none";
  }
  case Diagnostics::Survived: {
    return "survived";
  }
  case Diagnostics::Killed: {
    return "killed";
  }
  case Diagnostics::All: {
    return "all";
  }
  }
}

ParallelizationConfig::ParallelizationConfig()
    : workers(0), testExecutionWorkers(0), mutantExecutionWorkers(0) {}

void ParallelizationConfig::normalize() {
  int defaultWorkers = std::max(std::thread::hardware_concurrency(), uint(1));
  if (workers == 0) {
    workers = defaultWorkers;
  }
  if (testExecutionWorkers == 0) {
    testExecutionWorkers = workers;
  }

  if (mutantExecutionWorkers == 0) {
    mutantExecutionWorkers = workers;
  }
}

ParallelizationConfig ParallelizationConfig::defaultConfig() {
  ParallelizationConfig config;
  config.normalize();
  return config;
}

CustomTestDefinition::CustomTestDefinition() = default;

CustomTestDefinition::CustomTestDefinition(std::string name, std::string method,
                                           std::string program,
                                           std::vector<std::string> arguments)
    : testName(std::move(name)), methodName(std::move(method)),
      programName(std::move(program)), callArguments(std::move(arguments)) {}

} // namespace mull