#include "dump/DumpVisitor.h"

#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;

DumpVisitor::DumpVisitor(ASTContext *Context) : Context(Context) {}

bool DumpVisitor::VisitFunctionDecl(FunctionDecl *FD) {
  if (!FD->isThisDeclarationADefinition())
    return true;
  if (!isFromMainFile(FD->getLocation()))
    return true;

  llvm::outs() << "[FunctionDecl] " << FD->getNameAsString()
               << locStr(FD->getLocation()) << "\n";
  return true;
}

bool DumpVisitor::VisitVarDecl(VarDecl *VD) {
  if (!isFromMainFile(VD->getLocation()))
    return true;

  llvm::outs() << "[VarDecl] " << VD->getNameAsString();
  if (!VD->getType().isNull()) {
    llvm::outs() << " : " << VD->getType().getAsString();
  }
  llvm::outs() << locStr(VD->getLocation()) << "\n";
  return true;
}

bool DumpVisitor::VisitEnumDecl(EnumDecl *ED) {
  if (!ED->isCompleteDefinition())
    return true;
  if (!isFromMainFile(ED->getLocation()))
    return true;

  llvm::outs() << "[EnumDecl] " << ED->getNameAsString()
               << locStr(ED->getLocation()) << "\n";
  return true;
}

bool DumpVisitor::VisitEnumConstantDecl(EnumConstantDecl *ECD) {
  if (!isFromMainFile(ECD->getLocation()))
    return true;

  llvm::outs() << "[EnumConstantDecl] " << ECD->getNameAsString();
  if (ECD->getInitVal().isSignedIntN(64)) {
    llvm::outs() << " = " << ECD->getInitVal().getSExtValue();
  }
  llvm::outs() << locStr(ECD->getLocation()) << "\n";
  return true;
}

bool DumpVisitor::VisitIfStmt(IfStmt *If) {
  if (!isFromMainFile(If->getIfLoc()))
    return true;

  llvm::outs() << "[IfStmt]" << locStr(If->getIfLoc()) << "\n";
  return true;
}

bool DumpVisitor::VisitForStmt(ForStmt *For) {
  if (!isFromMainFile(For->getForLoc()))
    return true;

  llvm::outs() << "[ForStmt]" << locStr(For->getForLoc()) << "\n";
  return true;
}

bool DumpVisitor::VisitWhileStmt(WhileStmt *While) {
  if (!isFromMainFile(While->getWhileLoc()))
    return true;

  llvm::outs() << "[WhileStmt]" << locStr(While->getWhileLoc()) << "\n";
  return true;
}

bool DumpVisitor::VisitReturnStmt(ReturnStmt *Ret) {
  if (!isFromMainFile(Ret->getReturnLoc()))
    return true;

  llvm::outs() << "[ReturnStmt]" << locStr(Ret->getReturnLoc()) << "\n";
  return true;
}

bool DumpVisitor::VisitBinaryOperator(BinaryOperator *BO) {
  if (!isFromMainFile(BO->getOperatorLoc()))
    return true;

  llvm::outs() << "[BinaryOperator] " << BO->getOpcodeStr()
               << locStr(BO->getOperatorLoc()) << "\n";
  return true;
}

bool DumpVisitor::VisitUnaryOperator(UnaryOperator *UO) {
  if (!isFromMainFile(UO->getOperatorLoc()))
    return true;

  llvm::outs() << "[UnaryOperator] "
               << UnaryOperator::getOpcodeStr(UO->getOpcode())
               << locStr(UO->getOperatorLoc()) << "\n";
  return true;
}

bool DumpVisitor::VisitArraySubscriptExpr(ArraySubscriptExpr *AE) {
  if (!isFromMainFile(AE->getExprLoc()))
    return true;

  llvm::outs() << "[ArraySubscriptExpr]" << locStr(AE->getExprLoc()) << "\n";
  return true;
}

bool DumpVisitor::VisitDeclRefExpr(DeclRefExpr *DRE) {
  if (!isFromMainFile(DRE->getLocation()))
    return true;

  llvm::outs() << "[DeclRefExpr] " << DRE->getNameInfo().getAsString()
               << locStr(DRE->getLocation()) << "\n";
  return true;
}

bool DumpVisitor::VisitIntegerLiteral(IntegerLiteral *IL) {
  if (!isFromMainFile(IL->getLocation()))
    return true;

  llvm::outs() << "[IntegerLiteral] "
               << llvm::toString(IL->getValue(), 10, true)
               << locStr(IL->getLocation()) << "\n";
  return true;
}

bool DumpVisitor::VisitCallExpr(CallExpr *CE) {
  if (!isFromMainFile(CE->getExprLoc()))
    return true;

  llvm::outs() << "[CallExpr] ";
  if (FunctionDecl *Callee = CE->getDirectCallee()) {
    llvm::outs() << Callee->getNameAsString();
  } else {
    llvm::outs() << "<indirect-or-unknown>";
  }
  llvm::outs() << locStr(CE->getExprLoc()) << "\n";
  return true;
}

bool DumpVisitor::isFromMainFile(SourceLocation Loc) const {
  if (!Context || Loc.isInvalid())
    return false;

  const SourceManager &SM = Context->getSourceManager();
  SourceLocation ExpansionLoc = SM.getExpansionLoc(Loc);
  return SM.isWrittenInMainFile(ExpansionLoc);
}

std::string DumpVisitor::shortenPath(const std::string &Path) const {
  std::size_t pos = Path.rfind("examples/");
  if (pos != std::string::npos)
    return Path.substr(pos);

  pos = Path.rfind("examples\\");
  if (pos != std::string::npos)
    return Path.substr(pos);

  std::size_t slash = Path.find_last_of("/\\");
  if (slash != std::string::npos)
    return Path.substr(slash + 1);

  return Path;
}

std::string DumpVisitor::locStr(SourceLocation Loc) const {
  if (!Context || Loc.isInvalid())
    return "";

  const SourceManager &SM = Context->getSourceManager();
  SourceLocation ExpansionLoc = SM.getExpansionLoc(Loc);
  PresumedLoc PLoc = SM.getPresumedLoc(ExpansionLoc);

  if (PLoc.isInvalid())
    return "";

  std::string Result = " @";
  Result += shortenPath(PLoc.getFilename());
  Result += ":";
  Result += std::to_string(PLoc.getLine());
  Result += ":";
  Result += std::to_string(PLoc.getColumn());
  return Result;
}