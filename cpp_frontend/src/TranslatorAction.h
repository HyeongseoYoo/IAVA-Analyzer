#pragma once

#include <memory> //for using std::unique_ptr

#include "clang/Frontend/FrontendAction.h" // ASTFrontendAction

namespace clang {
class ASTConsumer;
class CompilerInstance;
} // namespace clang

class TranslatorAction : public clang::ASTFrontendAction {
public:
  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI,
                    llvm::StringRef InFile) override;
};