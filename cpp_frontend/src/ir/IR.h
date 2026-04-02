#pragma once

#include <string>
#include <vector>

namespace ir {

struct Expr {
  // kind-based 구조로 시작
  enum Kind { Int, Var, BinOp } kind;

  // 간단하게 union 느낌으로
  long long int_value;
  std::string var_name;

  struct {
    std::string op;
    Expr *lhs;
    Expr *rhs;
  } binop;
};

struct Stmt {
  enum Kind { Return, ExprStmt, If } kind;

  Expr *expr;

  struct {
    Expr *cond;
    Stmt *then_branch;
    Stmt *else_branch;
  } if_stmt;
};

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
  Stmt *body;
  std::string loc;
};

struct Program {
  std::vector<Var> globals;
  std::vector<Enum> enums;
  std::vector<Function> functions;
};



} // namespace ir