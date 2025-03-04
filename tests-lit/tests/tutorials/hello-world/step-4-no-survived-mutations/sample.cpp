// clang-format off

/**
RUN: cd / && %clang_cc %sysroot -fembed-bitcode -g -O0 %s -o %s.exe
RUN: cd %CURRENT_DIR
RUN: (unset TERM; %mull_cxx -linker=%clang_cxx -linker-flags="%sysroot" -mutators=cxx_ge_to_lt -mutators=cxx_ge_to_gt -ide-reporter-show-killed %s.exe 2>&1; test $? = 0) | %filecheck %s --dump-input=fail --strict-whitespace --match-full-lines
CHECK:[info] Killed mutants (2/2):
CHECK:{{^.*}}sample.cpp:15:11: warning: Killed: Replaced >= with > [cxx_ge_to_gt]{{$}}
CHECK:{{^.*}}sample.cpp:15:11: warning: Killed: Replaced >= with < [cxx_ge_to_lt]{{$}}
CHECK:[info] All mutations have been killed
CHECK:[info] Mutation score: 100%
**/

bool valid_age(int age) {
  if (age >= 21) {
    return true;
  }
  return false;
}

int main() {
  bool test1 = valid_age(25) == true;
  if (!test1) {
    /// test failed
    return 1;
  }

  bool test2 = valid_age(20) == false;
  if (!test2) {
    /// test failed
    return 1;
  }

  bool test3 = valid_age(21) == true;
  if (!test3) {
     /// test failed
     return 1;
  }

  /// success
  return 0;
}
