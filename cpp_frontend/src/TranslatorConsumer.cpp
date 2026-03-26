#include "TranslatorConsumer.h"

#include "Translator.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"

TranslatorConsumer::TranslatorConsumer(clang::ASTContext *Context)
    : Context(Context) {}

void TranslatorConsumer::HandleTranslationUnit(clang::ASTContext &Context) {
  auto *TU = Context.getTranslationUnitDecl();

  Translator translator(Context);
  ir::Program program = translator.translateTranslationUnit(TU);

  llvm::outs() << "[IR summary] "
               << "globals=" << program.globals.size()
               << ", enums=" << program.enums.size()
               << ", functions=" << program.functions.size() << "\n";
}