#pragma once

#include "ExecutionResult.h"
#include "MutationPoint.h"
#include <utility>

namespace mull {

class MutationResult {
  ExecutionResult Result;
  const MutationPoint *MutPoint;

public:
  MutationResult(ExecutionResult R, const MutationPoint *MP) : Result(std::move(R)), MutPoint(MP) {}

  ExecutionResult &getExecutionResult() {
    return Result;
  }
  const MutationPoint *getMutationPoint() {
    return MutPoint;
  }
};

} // namespace mull
