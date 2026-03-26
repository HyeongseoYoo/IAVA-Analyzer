#pragma once

#include <memory>

#include "clang/Frontend/FrontendAction.h"

class DumpAction : public clang::ASTFrontendAction {
public:
  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI,
                    llvm::StringRef InFile) override;
};