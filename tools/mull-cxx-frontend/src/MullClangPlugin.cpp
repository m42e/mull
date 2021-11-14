#include "ASTInstrumentation.h"
#include "ASTMutationsSearchVisitor.h"
#include "ASTNodeFactory.h"
#include "MullASTMutator.h"
#include "MutationMap.h"

#include <clang/AST/AST.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendPluginRegistry.h>
#include <clang/Sema/Sema.h>
#include <clang/Sema/SemaConsumer.h>
#include <llvm/Support/raw_ostream.h>

using namespace clang;
using namespace llvm;

namespace mull {
namespace cxx {

class MullASTConsumer : public ASTConsumer {
  CompilerInstance &instance;
  std::unique_ptr<MullASTMutator> astMutator;
  MutationMap mutationMap;

public:
  MullASTConsumer(CompilerInstance &instance, const MutationMap mutationMap)
      : instance(instance), astMutator(nullptr), mutationMap(mutationMap) {}

  void Initialize(ASTContext &Context) override {
    ASTConsumer::Initialize(Context);
  }

  /// This function can be considered a main() function of the
  /// mull-cxx-frontend plugin. This method is called multiple times by
  /// clang::ParseAST() for each declaration when it's finished being parsed.
  /// For each found function declaration below, a two-pass approach is used:
  /// 1) First all mutation points are found in the function declaration by the
  /// recursive AST visitor class ASTMutationsSearchVisitor.
  /// 2) For each mutation point, the mutations are performed on the Clang AST
  /// level. The mutation is performed by the higher-level MullASTMutator class
  /// which class to the lower-level ClangASTMutator class.
  bool HandleTopLevelDecl(DeclGroupRef DG) override {
    /// Could be a better place to create this. But at Initialize(), getSema()
    /// hits an internal assert because it is not initialized yet at that time.
    if (!astMutator) {
      assert(instance.getASTContext().getTranslationUnitDecl() && "WIP");

      astMutator = std::make_unique<MullASTMutator>(instance.getASTContext(), instance.getSema());
      astMutator->instrumentTranslationUnit();
    }

    for (DeclGroupRef::iterator I = DG.begin(), E = DG.end(); I != E; ++I) {
      if ((*I)->getKind() != Decl::Function) {
        continue;
      }

      FunctionDecl *f = static_cast<FunctionDecl *>(*I);
      if (f->getDeclName().getAsString() == "main") {
        continue;
      }

      clang::SourceLocation functionLocation = f->getLocation();
      if (instance.getSourceManager().isInSystemHeader(functionLocation)) {
        continue;
      }
      std::string sourceFilePath = instance.getSourceManager().getFilename(functionLocation).str();
      if (sourceFilePath.find("include/gtest") != std::string::npos) {
        continue;
      }
      ASTMutationsSearchVisitor visitor(instance.getASTContext(), mutationMap);
      errs() << "HandleTopLevelDecl: Looking at function: " << f->getDeclName() << "\n";
      visitor.TraverseFunctionDecl(f);

      for (auto &foundMutation : visitor.getAstMutations()) {
        foundMutation->performMutation(*astMutator);
        // The following is useful for debugging mutations:
        instance.getASTContext().getTranslationUnitDecl()->print(llvm::errs(), 2);
        instance.getASTContext().getTranslationUnitDecl()->dump();
//        exit(3);
      }

    }

    return true;
  }

  // This method is the last to be called when all declarations have already
  // been called on with HandleTopLevelDecl(). At this point, it is possible to
  // visualize the final mutated AST tree.
  void HandleTranslationUnit(ASTContext &context) override {

  }
};

class MullAction : public PluginASTAction {
  MutationMap mutationMap;

protected:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, llvm::StringRef) override {
    return std::make_unique<MullASTConsumer>(CI, mutationMap);
  }

  bool ParseArgs(const CompilerInstance &CI, const std::vector<std::string> &args) override {
    clang::ASTContext &astContext = CI.getASTContext();
    for (const auto &arg : args) {
      std::string delimiter = "=";
      std::vector<std::string> components;
      size_t last = 0;
      size_t next = 0;
      while ((next = arg.find(delimiter, last)) != std::string::npos) {
        components.push_back(arg.substr(last, next - last));
        last = next + 1;
      }
      components.push_back(arg.substr(last));
      if (components[0] != "mutators") {
        clang::DiagnosticsEngine &diag = astContext.getDiagnostics();
        unsigned diagId = diag.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                               "Only 'mutator=' argument is supported.");
        astContext.getDiagnostics().Report(diagId);
      }
      assert(components.size() == 2);
      mutationMap.addMutation(components.at(1));
    }
    mutationMap.setDefaultMutationsIfNotSpecified();
    return true;
  }

  PluginASTAction::ActionType getActionType() override {
    /// Note: AddBeforeMainAction is the only option when mutations have effect.
    return AddBeforeMainAction;
  }
};

} // namespace cxx
} // namespace mull

static FrontendPluginRegistry::Add<mull::cxx::MullAction> X("mull-cxx-frontend",
                                                            "Mull: Prepare mutations");
