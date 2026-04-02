#include "ir/IRPrinter.h"

#include "llvm/Support/raw_ostream.h"

namespace ir {

void IRPrinter::print(const Program &program) {
  llvm::outs() << "=== IR Program ===\n\n";

  printGlobals(program);
  printEnums(program);
  printFunctions(program);
}

void IRPrinter::printGlobals(const Program &program) {
  llvm::outs() << "Globals:\n";

  for (const auto &var : program.globals) {
    llvm::outs() << "  " << var.name;

    if (!var.type.empty()) {
      llvm::outs() << " : " << var.type;
    }

    if (!var.loc.empty()) {
      llvm::outs() << " @" << var.loc;
    }

    llvm::outs() << "\n";
  }

  llvm::outs() << "\n";
}

void IRPrinter::printEnums(const Program &program) {
  llvm::outs() << "Enums:\n";

  for (const auto &e : program.enums) {
    llvm::outs() << "  enum " << e.name;

    if (!e.loc.empty()) {
      llvm::outs() << " @" << e.loc;
    }

    llvm::outs() << "\n";

    for (const auto &enumerator : e.enumerators) {
      llvm::outs() << "    " << enumerator.name << " = " << enumerator.value;

      if (!enumerator.loc.empty()) {
        llvm::outs() << " @" << enumerator.loc;
      }

      llvm::outs() << "\n";
    }
  }

  llvm::outs() << "\n";
}

void IRPrinter::printFunctions(const Program &program) {
  llvm::outs() << "Functions:\n";

  for (const auto &func : program.functions) {
    llvm::outs() << "  func " << func.name << "(";

    // params
    for (size_t i = 0; i < func.params.size(); ++i) {
      const auto &p = func.params[i];
      llvm::outs() << p.type << " " << p.name;

      if (i + 1 < func.params.size()) {
        llvm::outs() << ", ";
      }
    }

    llvm::outs() << ")";

    if (!func.return_type.empty()) {
      llvm::outs() << " -> " << func.return_type;
    }

    if (!func.loc.empty()) {
      llvm::outs() << " @" << func.loc;
    }

    llvm::outs() << "\n";
  }

  llvm::outs() << "\n";
}

} // namespace ir