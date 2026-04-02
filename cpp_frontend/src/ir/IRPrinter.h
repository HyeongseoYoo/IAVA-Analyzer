#pragma once

#include "ir/IR.h"

namespace ir {

class IRPrinter {
public:
  static void print(const Program &program);

private:
  static void printGlobals(const Program &program);
  static void printEnums(const Program &program);
  static void printFunctions(const Program &program);
};

} // namespace ir