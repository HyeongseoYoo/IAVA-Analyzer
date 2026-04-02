#include "translator/TranslatorConsumer.h"

#include "ir/IRPrinter.h"
#include "translator/Translator.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"

TranslatorConsumer::TranslatorConsumer(clang::ASTContext *Context)
    : Context(Context) {}

void TranslatorConsumer::HandleTranslationUnit(clang::ASTContext &Context) {
  auto *TU = Context.getTranslationUnitDecl();

  Translator translator(Context);
  ir::Program program = translator.translateTranslationUnit(TU);

  ir::IRPrinter::print(program);
}