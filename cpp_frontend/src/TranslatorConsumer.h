#pragma once

#include "clang/AST/ASTConsumer.h"

namespace clang {
class ASTContext;
}

class TranslatorConsumer : public clang::ASTConsumer {
public:
  explicit TranslatorConsumer(clang::ASTContext *Context);

  void HandleTranslationUnit(clang::ASTContext &Context) override;

private:
  clang::ASTContext *Context;
};