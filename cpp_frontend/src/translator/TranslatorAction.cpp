#include "translator/TranslatorAction.h"

#include <memory>

#include "translator/TranslatorConsumer.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/Frontend/CompilerInstance.h"

std::unique_ptr<clang::ASTConsumer>
TranslatorAction::CreateASTConsumer(clang::CompilerInstance &CI,
                                    llvm::StringRef InFile) {
  return std::make_unique<TranslatorConsumer>(&CI.getASTContext());
}