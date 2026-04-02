#include <memory>

#include "dump/DumpAction.h"
#include "translator/TranslatorAction.h"

#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"

using namespace clang::tooling;
using namespace llvm;

static cl::OptionCategory ToolCategory("cpp_frontend options");

static cl::opt<bool>
    DumpAstLike("dump-ast-like",
                cl::desc("Dump selected AST nodes for debugging"),
                cl::init(false), cl::cat(ToolCategory));

int main(int argc, const char **argv) {
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, ToolCategory);
  if (!ExpectedParser) {
    llvm::errs() << "Failed to parse options.\n";
    return 1;
  }

  CommonOptionsParser &OptionsParser = ExpectedParser.get();
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  if (DumpAstLike) {
    return Tool.run(newFrontendActionFactory<DumpAction>().get());
  }

  return Tool.run(newFrontendActionFactory<TranslatorAction>().get());
}