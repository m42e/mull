#pragma once

#include "mull/Bitcode.h"
#include <string>
#include <unordered_map>
#include <vector>

namespace mull {

class MutationPoint;
class progress_counter;

class CloneMutatedFunctionsTask {
public:
  using In = std::vector<std::unique_ptr<Bitcode>>;
  using Out = std::vector<int>;
  using iterator = In::const_iterator;

  CloneMutatedFunctionsTask() = default;

  void operator()(iterator begin, iterator end, Out &storage,
                  progress_counter &counter);
  static void cloneFunctions(Bitcode &bitcode);

private:
};

class DeleteOriginalFunctionsTask {
public:
  using In = std::vector<std::unique_ptr<Bitcode>>;
  using Out = std::vector<int>;
  using iterator = In::const_iterator;

  DeleteOriginalFunctionsTask() = default;

  void operator()(iterator begin, iterator end, Out &storage,
                  progress_counter &counter);
  static void deleteFunctions(Bitcode &bitcode);

private:
};

class InsertMutationTrampolinesTask {
public:
  using In = std::vector<std::unique_ptr<Bitcode>>;
  using Out = std::vector<int>;
  using iterator = In::const_iterator;

  explicit InsertMutationTrampolinesTask(const std::unordered_map<std::string, size_t> &mutants);

  void operator()(iterator begin, iterator end, Out &storage,
                  progress_counter &counter);
  void insertTrampolines(Bitcode &bitcode);

  static void insertRT(Bitcode &bitcode, const std::unordered_map<std::string, size_t> &mutants);

private:
  const std::unordered_map<std::string, size_t> &mutants;
};

} // namespace mull