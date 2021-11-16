set (SOURCES
  ${CMAKE_CURRENT_LIST_DIR}/mull-cxx.cpp
  ${CMAKE_CURRENT_LIST_DIR}/../CLIOptions/CLIOptions.cpp
)
message("${LLVM_NATIVE_ARCH}")
if ("LLVM" IN_LIST LLVM_AVAILABLE_LIBS)
  set (MULL_CXX_LLVM_LIBRARIES
    LLVM
    LLVMX86Info
    LLVMX86AsmParser
    LLVMX86CodeGen
    LLVMX86Desc
  )
else()
  set (MULL_CXX_LLVM_LIBRARIES
    clangAST
    clangBasic
    clangFrontend
    clangLex
    LLVMBitReader
    LLVMBitWriter
    LLVMDemangle
    LLVMObject
    LLVMSupport
    LLVMTarget
    LLVMX86CodeGen
    LLVMX86AsmParser
    LLVMX86Desc
    LLVMX86Info
    LLVMTransformUtils
  )
endif()

add_mull_executable(
  NAME mull-cxx-${LLVM_VERSION_MAJOR}
  SOURCES ${SOURCES}
  LINK_WITH ebc mull ${MULL_CXX_LLVM_LIBRARIES} json11
)
target_include_directories(mull-cxx-${LLVM_VERSION_MAJOR} PRIVATE ${CMAKE_CURRENT_LIST_DIR}/../CLIOptions)
