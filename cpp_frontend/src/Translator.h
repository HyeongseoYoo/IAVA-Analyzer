#pragma once

#include <string>

#include "IR.h"

namespace clang {
class ASTContext;
class TranslationUnitDecl;
class FunctionDecl;
class VarDecl;
class EnumDecl;
class SourceLocation;
} // namespace clang

class Translator {
public:
  explicit Translator(clang::ASTContext &Context);

  ir::Program translateTranslationUnit(clang::TranslationUnitDecl *TU);

private:
  clang::ASTContext &Context;

  bool isFromMainFile(clang::SourceLocation Loc) const;
  std::string shortenPath(const std::string &Path) const;
  std::string locStr(clang::SourceLocation Loc) const;

  ir::Function translateFunctionDecl(clang::FunctionDecl *FD);
  ir::Var translateVarDecl(clang::VarDecl *VD);
  ir::Enum translateEnumDecl(clang::EnumDecl *ED);
};