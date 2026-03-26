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
  src/DumpAction.cpp
  src/DumpVisitor.cpp
  src/TranslatorAction.cpp
  src/TranslatorConsumer.cpp
  src/Translator.cpp
)

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