#include "translator/Translator.h"
#include "ir/IR.h"

#include <string>

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/Support/Casting.h"

#include "lowering/AstToIr.h"

Translator::Translator(clang::ASTContext &Context) : Context(Context) {}

ir::Program Translator::translateTranslationUnit(clang::TranslationUnitDecl *TU) {
  ir::Program program;

  // Only top-level declarations are handled here. Nested declarations (e.g., local variables)
  // are handled within their respective parent declarations (e.g., function bodies).
  for (auto *Decl : TU->decls()) {
    if (auto *FD = llvm::dyn_cast<clang::FunctionDecl>(Decl)) {
      if (!FD->isThisDeclarationADefinition())
        continue;

      if (!isFromMainFile(FD->getLocation()))
        continue;

      program.functions.push_back(translateFunctionDecl(FD));
      continue;
    }

    if (auto *VD = llvm::dyn_cast<clang::VarDecl>(Decl)) {
      if (!isFromMainFile(VD->getLocation()))
        continue;

      if (!VD->hasGlobalStorage())
        continue;

      program.globals.push_back(translateVarDecl(VD));
      continue;
    }

    if (auto *ED = llvm::dyn_cast<clang::EnumDecl>(Decl)) {
      if (!ED->isCompleteDefinition())
        continue;

      if (!isFromMainFile(ED->getLocation()))
        continue;

      program.enums.push_back(translateEnumDecl(ED));
      continue;
    }
  }

  return program;
}

ir::Function Translator::translateFunctionDecl(clang::FunctionDecl *FD) {
  ir::Function function;
  function.name = FD->getNameAsString();
  function.return_type = FD->getReturnType().getAsString();
  function.has_body = FD->hasBody();
  function.loc = locStr(FD->getLocation());

  for (auto *Param : FD->parameters()) {
    ir::Param param;
    param.name = Param->getNameAsString();
    param.type = Param->getType().getAsString();
    function.params.push_back(param);
  }

  if (FD->hasBody()) { // body 추가하는 부분부터 다시 봐야함
    AstToIr lowering(Context);
    function.body = lowering.translateStmt(FD->getBody());
  }

  return function;
}

ir::Var Translator::translateVarDecl(clang::VarDecl *VD) {
  ir::Var var;
  var.name = VD->getNameAsString();
  var.type = VD->getType().getAsString();
  var.loc = locStr(VD->getLocation());

  return var;
}

ir::Enum Translator::translateEnumDecl(clang::EnumDecl *ED) {
  ir::Enum enum_decl;
  enum_decl.name = ED->getNameAsString();
  enum_decl.loc = locStr(ED->getLocation());

  for (auto *ECD : ED->enumerators()) {
    ir::Enumerator enumerator;
    enumerator.name = ECD->getNameAsString();
    enumerator.value = ECD->getInitVal().getSExtValue();
    enumerator.loc = locStr(ECD->getLocation());
    enum_decl.enumerators.push_back(enumerator);
  }

  return enum_decl;
}


bool Translator::isFromMainFile(clang::SourceLocation Loc) const {
  if (Loc.isInvalid())
    return false;

  const clang::SourceManager &SM = Context.getSourceManager();
  clang::SourceLocation ExpansionLoc = SM.getExpansionLoc(Loc);
  return SM.isWrittenInMainFile(ExpansionLoc);
}

std::string Translator::shortenPath(const std::string &Path) const {
  std::size_t Pos = Path.rfind("examples/");
  if (Pos != std::string::npos)
    return Path.substr(Pos);

  Pos = Path.rfind("examples\\");
  if (Pos != std::string::npos)
    return Path.substr(Pos);

  std::size_t Slash = Path.find_last_of("/\\");
  if (Slash != std::string::npos)
    return Path.substr(Slash + 1);

  return Path;
}

std::string Translator::locStr(clang::SourceLocation Loc) const {
  if (Loc.isInvalid())
    return "";

  const clang::SourceManager &SM = Context.getSourceManager();
  clang::SourceLocation ExpansionLoc = SM.getExpansionLoc(Loc);
  clang::PresumedLoc PLoc = SM.getPresumedLoc(ExpansionLoc);

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