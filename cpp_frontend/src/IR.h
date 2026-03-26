#pragma once

#include <string>
#include <vector>

namespace ir {

struct Var {
  std::string name;
  std::string type;
  std::string loc; // e.g., "sample.cpp:10:5"
};

struct Enumerator {
  std::string name;
  long long value;
  std::string loc;
};

struct Enum {
  std::string name;
  std::vector<Enumerator> enumerators;
  std::string loc;
};

struct Param {
  std::string name;
  std::string type;
};

struct Function {
  std::string name;
  std::string return_type;
  std::vector<Param> params;
  bool has_body;
  std::string loc;
};

struct Program {
  std::vector<Var> globals;
  std::vector<Enum> enums;
  std::vector<Function> functions;
};

} // namespace ir