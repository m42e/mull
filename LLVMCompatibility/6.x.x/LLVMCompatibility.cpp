#include "LLVMCompatibility.h"

#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/DebugLoc.h>
#include <llvm/Object/ObjectFile.h>

using namespace llvm;

namespace llvm_compat {

JITSymbolFlags JITSymbolFlagsFromObjectSymbol(const object::BasicSymbolRef &symbol) {
  return JITSymbolFlags::fromObjectSymbol(symbol);
}

uint64_t JITSymbolAddress(JITSymbol &symbol) {
  auto addressOrError = symbol.getAddress();
  if (!addressOrError) {
    consumeError(addressOrError.takeError());
    return 0;
  }

  return addressOrError.get();
}

object::OwningBinary<object::ObjectFile> compileModule(orc::SimpleCompiler &compiler,
                                                       llvm::Module &module) {
  return compiler(module);
}

std::unique_ptr<Module> parseBitcode(MemoryBufferRef bufferRef, LLVMContext &context) {
  auto module = parseBitcodeFile(bufferRef, context);
  if (!module) {
    logAllUnhandledErrors(module.takeError(), errs(), "\nparseBitcodeFile failed: ");
    return std::unique_ptr<Module>();
  }

  return std::move(module.get());
}

void setVersionPrinter(void (*oldPrinter)(), void (*newPrinter)(raw_ostream &)) {
  llvm::cl::SetVersionPrinter(newPrinter);
}

StringRef getSectionContent(const object::SectionRef &section) {
  StringRef content;
  section.getContents(content);
  return content;
}
StringRef getSectionName(const object::SectionRef &section) {
  StringRef name;
  section.getName(name);
  return name;
}

DICompileUnit *getUnit(const DebugLoc &debugLocation) {
  DIScope *scope = debugLocation->getScope();
  while (!llvm::isa<llvm::DISubprogram>(scope) && scope != nullptr) {
    scope = scope->getScope().resolve();
  }
  return scope ? llvm::cast<llvm::DISubprogram>(scope)->getUnit() : nullptr;
}

bool parseCommandLineOptions(int argc, char **argv) {
  return llvm::cl::ParseCommandLineOptions(argc, argv, "", &llvm::errs());
}

} // namespace llvm_compat
