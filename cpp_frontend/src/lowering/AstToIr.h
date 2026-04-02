#pragma once

#include "ir/IR.h"

namespace clang {
class ASTContext;
class Stmt;
class Expr;
class FunctionDecl;
} // namespace clang

class AstToIr {
public:
  explicit AstToIr(clang::ASTContext &Context);

  ir::Stmt *translateStmt(clang::Stmt *S);
  ir::Expr *translateExpr(clang::Expr *E);

private:
  clang::ASTContext &Context;
};