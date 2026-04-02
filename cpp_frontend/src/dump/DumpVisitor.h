#pragma once

#include <string>

#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"

class DumpVisitor : public clang::RecursiveASTVisitor<DumpVisitor> {
public:
  explicit DumpVisitor(clang::ASTContext *Context);

  bool VisitFunctionDecl(clang::FunctionDecl *FD);
  bool VisitVarDecl(clang::VarDecl *VD);
  bool VisitEnumDecl(clang::EnumDecl *ED);
  bool VisitEnumConstantDecl(clang::EnumConstantDecl *ECD);
  bool VisitIfStmt(clang::IfStmt *If);
  bool VisitForStmt(clang::ForStmt *For);
  bool VisitWhileStmt(clang::WhileStmt *While);
  bool VisitReturnStmt(clang::ReturnStmt *Ret);
  bool VisitBinaryOperator(clang::BinaryOperator *BO);
  bool VisitUnaryOperator(clang::UnaryOperator *UO);
  bool VisitArraySubscriptExpr(clang::ArraySubscriptExpr *AE);
  bool VisitDeclRefExpr(clang::DeclRefExpr *DRE);
  bool VisitIntegerLiteral(clang::IntegerLiteral *IL);
  bool VisitCallExpr(clang::CallExpr *CE);

private:
  clang::ASTContext *Context;

  bool isFromMainFile(clang::SourceLocation Loc) const;
  std::string shortenPath(const std::string &Path) const;
  std::string locStr(clang::SourceLocation Loc) const;
};