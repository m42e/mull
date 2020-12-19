#include "mull/Parallelization/Tasks/MutantPreparationTasks.h"
#include "LLVMCompatibility.h"
#include "mull/MutationPoint.h"
#include "mull/Parallelization/Progress.h"
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include <unordered_map>
#include <unordered_set>

using namespace mull;

void CloneMutatedFunctionsTask::operator()(iterator begin, iterator end, Out &storage,
                                           progress_counter &counter) {
  for (auto it = begin; it != end; it++, counter.increment()) {
    Bitcode &bitcode = **it;
    cloneFunctions(bitcode);
  }
}

void CloneMutatedFunctionsTask::cloneFunctions(Bitcode &bitcode) {
  for (auto &pair : bitcode.getMutationPointsMap()) {
    llvm::Function *original = pair.first;
    for (MutationPoint *point : pair.second) {
      llvm::ValueToValueMapTy map;
      llvm::Function *mutatedFunction = llvm::CloneFunction(original, map);
      point->setMutatedFunction(mutatedFunction);
    }
  }
}

void DeleteOriginalFunctionsTask::operator()(iterator begin, iterator end, Out &storage,
                                             progress_counter &counter) {
  for (auto it = begin; it != end; it++, counter.increment()) {
    Bitcode &bitcode = **it;
    deleteFunctions(bitcode);
  }
}

void DeleteOriginalFunctionsTask::deleteFunctions(Bitcode &bitcode) {
  for (auto pair : bitcode.getMutationPointsMap()) {
    auto original = pair.first;
    auto anyPoint = pair.second.front();

    llvm::ValueToValueMapTy map;
    auto originalCopy = CloneFunction(original, map);
    originalCopy->setName(anyPoint->getOriginalFunctionName());
    original->deleteBody();
  }
}

void InsertMutationTrampolinesTask::operator()(iterator begin, iterator end, Out &storage,
                                               progress_counter &counter) {
  for (auto it = begin; it != end; it++, counter.increment()) {
    Bitcode &bitcode = **it;
    insertTrampolines(bitcode);
  }
}

void InsertMutationTrampolinesTask::insertTrampolines(Bitcode &bitcode) {
  llvm::Module *module = bitcode.getModule();
  llvm::LLVMContext &context = module->getContext();

  auto *currentMutant = new llvm::GlobalVariable(*module,
                                                 llvm::Type::getInt64Ty(context),
                                                 false,
                                                 llvm::GlobalVariable::ExternalLinkage,
                                                 nullptr,
                                                 "currentMutant");
  //  llvm::errs() << "\n";

  //  llvm::Type *charPtr = llvm::Type::getInt8Ty(context)->getPointerTo();
  //  llvm::FunctionType *getEnvType = llvm::FunctionType::get(charPtr, { charPtr }, false);
  //  llvm::Value *getenv = llvm_compat::getOrInsertFunction(module, "getenv", getEnvType);

  for (auto pair : bitcode.getMutationPointsMap()) {
    llvm::Function *original = pair.first;

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", original);
    llvm::BasicBlock *originalBlock = llvm::BasicBlock::Create(context, "original", original);
    llvm::BasicBlock *trampolineCall =
        llvm::BasicBlock::Create(context, "trampoline_call", original);

    auto anyPoint = pair.second.front();
    llvm::Type *trampolineType = original->getFunctionType()->getPointerTo();
    auto trampoline = new llvm::AllocaInst(trampolineType, 0, "trampoline", entry);

    new llvm::StoreInst(bitcode.getModule()->getFunction(anyPoint->getOriginalFunctionName()),
                        trampoline,
                        originalBlock);
    llvm::BranchInst::Create(trampolineCall, originalBlock);

    auto currentMutantValue = new llvm::LoadInst(currentMutant->getType()->getPointerElementType(),
                                                 currentMutant,
                                                 "currentMutant",
                                                 false,
                                                 entry);
    llvm::SwitchInst *jumpTable =
        llvm::SwitchInst::Create(currentMutantValue, originalBlock, pair.second.size(), entry);
    //    std::unordered_set<MutationPoint *> uniquePoints;
    //    for (auto point : pair.second) {
    //      uniquePoints.insert(point);
    //    }

    std::unordered_map<std::string, std::string> muttants;
    for (auto &point : pair.second) {
      muttants[point->getUserIdentifier()] = point->getMutatedFunctionName();
    }

    for (auto &ppair : muttants) {
      auto userIdentifier = ppair.first;
      auto functionName = ppair.second;
      llvm::BasicBlock *mutationBlock = llvm::BasicBlock::Create(context, userIdentifier, original);
      new llvm::StoreInst(
          bitcode.getModule()->getFunction(functionName), trampoline, mutationBlock);
      jumpTable->addCase(
          llvm::ConstantInt::get(context, llvm::APInt(64, mutants.at(userIdentifier))),
          mutationBlock);
      llvm::BranchInst::Create(trampolineCall, mutationBlock);
    }

    //    llvm::BasicBlock *head = originalBlock;
    //
    //    for (auto &point : pair.second) {
    //      auto name = llvm::ConstantDataArray::getString(context, point->getUserIdentifier());
    //      auto *global = new llvm::GlobalVariable(
    //          *module, name->getType(), true, llvm::GlobalValue::PrivateLinkage, name);
    //
    //      llvm::BasicBlock *mutationBlock =
    //          llvm::BasicBlock::Create(context, point->getUserIdentifier(), original);
    //      new llvm::StoreInst(bitcode.getModule()->getFunction(point->getMutatedFunctionName()),
    //                          trampoline,
    //                          mutationBlock);
    //      llvm::Value *nullPtr = llvm::Constant::getNullValue(charPtr);
    //      llvm::CmpInst *predicate = llvm::CmpInst::Create(llvm::Instruction::ICmp,
    //                                                       llvm::ICmpInst::ICMP_NE,
    //                                                       nullPtr,
    //                                                       nullPtr,
    //                                                       "is_enabled",
    //                                                       mutationBlock);
    //      llvm::Value *zero = llvm::Constant::getNullValue(llvm::Type::getInt64Ty(context));
    //      auto mutantName =
    //          llvm::ConstantExpr::getInBoundsGetElementPtr(name->getType(), global, { zero, zero
    //          });
    //      auto getEnvCall =
    //          llvm::CallInst::Create(getEnvType, getenv, { mutantName }, "check_mutation",
    //          predicate);
    //      predicate->setOperand(0, getEnvCall);
    //      llvm::BranchInst::Create(trampolineCall, head, predicate, mutationBlock);
    //      head = mutationBlock;
    //    }
    //
    //    llvm::BranchInst::Create(head, entry);

    std::vector<llvm::Value *> args;
    for (auto &arg : original->args()) {
      args.push_back(&arg);
    }

    auto retType = original->getFunctionType()->getReturnType();
    llvm::Constant *dummy = nullptr;
    if (!retType->isVoidTy()) {
      dummy = llvm::Constant::getNullValue(retType);
    }

    auto retVal = llvm::ReturnInst::Create(context, dummy, trampolineCall);
    auto loadValue = new llvm::LoadInst(trampoline->getType()->getPointerElementType(),
                                        trampoline,
                                        "trampoline_pointer",
                                        false,
                                        retVal);
    auto callInst =
        llvm::CallInst::Create(original->getFunctionType(), loadValue, args, "", retVal);
    if (!retType->isVoidTy()) {
      retVal->setOperand(0, callInst);
    }
  }
}

InsertMutationTrampolinesTask::InsertMutationTrampolinesTask(
    const std::unordered_map<std::string, size_t> &mutants)
    : mutants(mutants) {}

void InsertMutationTrampolinesTask::insertRT(
    Bitcode &bitcode, const std::unordered_map<std::string, size_t> &mutants) {
  llvm::Module *module = bitcode.getModule();
  llvm::LLVMContext &context = module->getContext();

  llvm::Type *charPtr = llvm::Type::getInt8Ty(context)->getPointerTo();
  llvm::FunctionType *getEnvType = llvm::FunctionType::get(charPtr, { charPtr }, false);
  llvm::Value *getenv = llvm_compat::getOrInsertFunction(module, "getenv", getEnvType);

  llvm::FunctionType *initMullRTType =
      llvm::FunctionType::get(llvm::Type::getVoidTy(context), {}, false);
  llvm::Function *initRT = llvm::cast<llvm::Function>(
      llvm_compat::getOrInsertFunction(module, "init_mull", initMullRTType));
  (void)initRT;
  (void)getenv;

  llvm::GlobalVariable *currentMutant = module->getGlobalVariable("currentMutant");
  currentMutant->setLinkage(llvm::GlobalVariable::InternalLinkage);
  currentMutant->setInitializer(llvm::ConstantInt::get(context, llvm::APInt(64, 0)));

  llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", initRT);
  llvm::BasicBlock *ret = llvm::BasicBlock::Create(context, "exit", initRT);
  llvm::ReturnInst::Create(context, ret);

  llvm::BasicBlock *noMutations = llvm::BasicBlock::Create(context, "no_mutations", initRT);
  new llvm::StoreInst(
      llvm::ConstantInt::get(context, llvm::APInt(64, 0)), currentMutant, noMutations);
  llvm::BranchInst::Create(ret, noMutations);

  llvm::BasicBlock *head = noMutations;

  for (auto &pair : mutants) {
    if (pair.second == 0) {
      /// Dummy
      continue;
    }
    auto userIdentifier = pair.first;
    auto name = llvm::ConstantDataArray::getString(context, userIdentifier);
    auto *global = new llvm::GlobalVariable(
        *module, name->getType(), true, llvm::GlobalValue::PrivateLinkage, name);

    llvm::BasicBlock *mutationBlock = llvm::BasicBlock::Create(context, userIdentifier, initRT);
    new llvm::StoreInst(llvm::ConstantInt::get(context, llvm::APInt(64, pair.second)),
                        currentMutant,
                        mutationBlock);
    llvm::Value *nullPtr = llvm::Constant::getNullValue(charPtr);
    llvm::CmpInst *predicate = llvm::CmpInst::Create(llvm::Instruction::ICmp,
                                                     llvm::ICmpInst::ICMP_NE,
                                                     nullPtr,
                                                     nullPtr,
                                                     "is_enabled",
                                                     mutationBlock);
    llvm::Value *zero = llvm::Constant::getNullValue(llvm::Type::getInt64Ty(context));
    auto mutantName =
        llvm::ConstantExpr::getInBoundsGetElementPtr(name->getType(), global, { zero, zero });
    auto getEnvCall =
        llvm::CallInst::Create(getEnvType, getenv, { mutantName }, "check_mutation", predicate);
    predicate->setOperand(0, getEnvCall);
    llvm::BranchInst::Create(ret, head, predicate, mutationBlock);
    head = mutationBlock;
  }

  llvm::BranchInst::Create(head, entry);
  llvm::appendToGlobalCtors(*module, initRT, 0);
  //  module->print(llvm::errs(), nullptr);
  //  exit(10);
}
