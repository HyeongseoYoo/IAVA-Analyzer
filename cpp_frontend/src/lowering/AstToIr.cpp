#include "lowering/AstToIr.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Stmt.h"

#include "llvm/Support/Casting.h"

AstToIr::AstToIr(clang::ASTContext &Context) : Context(Context) {}

ir::Expr *AstToIr::translateExpr(clang::Expr *E) {
  E = E->IgnoreImpCasts();

  if (auto *IL = llvm::dyn_cast<clang::IntegerLiteral>(E)) {
    auto *expr = new ir::Expr();
    expr->kind = ir::Expr::Int;
    expr->int_value = IL->getValue().getSExtValue();
    return expr;
  }

  if (auto *DRE = llvm::dyn_cast<clang::DeclRefExpr>(E)) {
    auto *expr = new ir::Expr();
    expr->kind = ir::Expr::Var;
    expr->var_name = DRE->getNameInfo().getAsString();
    return expr;
  }

  if (auto *BO = llvm::dyn_cast<clang::BinaryOperator>(E)) {
    auto *expr = new ir::Expr();
    expr->kind = ir::Expr::BinOp;
    expr->binop.op = BO->getOpcodeStr().str();
    expr->binop.lhs = translateExpr(BO->getLHS());
    expr->binop.rhs = translateExpr(BO->getRHS());
    return expr;
  }

  return nullptr; // unsupported
}

ir::Stmt *AstToIr::translateStmt(clang::Stmt *S) {
  if (auto *RS = llvm::dyn_cast<clang::ReturnStmt>(S)) {
    auto *stmt = new ir::Stmt();
    stmt->kind = ir::Stmt::Return;
    stmt->expr = translateExpr(RS->getRetValue());
    return stmt;
  }

  if (auto *ES = llvm::dyn_cast<clang::Expr>(S)) {
    auto *stmt = new ir::Stmt();
    stmt->kind = ir::Stmt::ExprStmt;
    stmt->expr = translateExpr(ES);
    return stmt;
  }

  if (auto *IF = llvm::dyn_cast<clang::IfStmt>(S)) {
    auto *stmt = new ir::Stmt();
    stmt->kind = ir::Stmt::If;
    stmt->if_stmt.cond = translateExpr(IF->getCond());
    stmt->if_stmt.then_branch = translateStmt(IF->getThen());
    stmt->if_stmt.else_branch =
        IF->getElse() ? translateStmt(IF->getElse()) : nullptr;
    return stmt;
  }

  return nullptr;
}