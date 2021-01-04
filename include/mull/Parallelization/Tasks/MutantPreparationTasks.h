#pragma once

namespace mull {

class Bitcode;

struct CloneMutatedFunctionsTask {
  static void cloneFunctions(Bitcode &bitcode);
};

struct DeleteOriginalFunctionsTask {
  static void deleteFunctions(Bitcode &bitcode);
};

struct InsertMutationTrampolinesTask {
  static void insertTrampolines(Bitcode &bitcode);
};

} // namespace mull