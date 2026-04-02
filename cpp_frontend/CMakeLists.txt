cmake_minimum_required(VERSION 3.16)
project(cpp_frontend LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Using ClangConfig.cmake in: ${Clang_DIR}")

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(cpp_frontend
  src/main.cpp

  src/dump/DumpAction.cpp
  src/dump/DumpVisitor.cpp

  src/translator/TranslatorAction.cpp
  src/translator/TranslatorConsumer.cpp
  src/translator/Translator.cpp

  src/ir/IRPrinter.cpp

  src/lowering/AstToIr.cpp
  src/lowering/Lowering.cpp
)

target_include_directories(cpp_frontend PRIVATE src)

target_link_libraries(cpp_frontend
  PRIVATE
  clangTooling
  clangBasic
  clangAST
  clangFrontend
  clangSerialization
  clangDriver
  clangParse
  clangSema
  clangEdit
  clangLex
  LLVM
)