#include "dump/DumpAction.h"

#include "dump/DumpVisitor.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/Frontend/CompilerInstance.h"

using namespace clang;

namespace {
class DumpConsumer : public ASTConsumer {
public:
  explicit DumpConsumer(ASTContext *Context) : Visitor(Context) {}

  void HandleTranslationUnit(ASTContext &Context) override {
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }

private:
  DumpVisitor Visitor;
};
} // namespace

std::unique_ptr<ASTConsumer>
DumpAction::CreateASTConsumer(CompilerInstance &CI, llvm::StringRef InFile) {
  return std::make_unique<DumpConsumer>(&CI.getASTContext());
}