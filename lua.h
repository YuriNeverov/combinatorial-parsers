#ifndef COMBINATORIAL_PARSERS_LUA_H_
#define COMBINATORIAL_PARSERS_LUA_H_

#include <deque>
#include <memory>
#include <string>
#include <vector>

namespace combinatorial_parsers {
namespace lua {

enum StatementKind {
  SK_Statement,
  SK_Chunk,
  SK_DoStatement,
  SK_WhileStatement,
  SK_RepeatStatement,
  SK_IfStatement,
  SK_ForStatement,
  SK_ForeachStatement,
  SK_AssignmentStatement,
  SK_Terminator,
  SK_ReturnStatement,
  SK_BreakStatement,
  SK_LastTerminator,
  SK_Expression,
  SK_Literal,
  SK_Number,
  SK_String,
  SK_Bool,
  SK_Nil,
  SK_Etc,
  SK_LastLiteral,
  SK_Operator,
  SK_UnaryOperator,
  SK_BinaryOperator,
  SK_LastOperator,
  SK_Reference,
  SK_Call,
  SK_Function,
  SK_Table,
  SK_LastExpression,
  SK_LastStatement,
};

enum UnaryOperatorKind {
  UO_Negate,
  UO_SizeOf,
  UO_LNot,
};

enum BinaryOperatorKind {
  BO_Add,
  BO_Subtract,
  BO_Multiply,
  BO_Divide,
  BO_Power,
  BO_Modulo,
  BO_Concat,
  BO_LessThan,
  BO_LessThanOrEqualsTo,
  BO_GreaterThan,
  BO_GreaterThanOrEqualsTo,
  BO_EqualsTo,
  BO_NotEqualsTo,
  BO_LAnd,
  BO_LOr,
  BO_Access,
  BO_FieldAccess,
  BO_SelfAccess,
};

inline void tab(std::string &s, size_t lvl) {
  for (size_t i = 0; i < lvl; i++) s += "  ";
}

struct Statement {
  Statement(StatementKind kind) : kind(kind) {}

  virtual std::string toString(size_t lvl) const = 0;

  virtual ~Statement() = default;

  StatementKind kind;
};

struct Expression : Statement {
  Expression(StatementKind kind) : Statement(kind) {}
};

struct Reference : Expression {
  Reference() : Expression(SK_Reference) {}
  Reference(std::string name)
      : Expression(SK_Reference), name(std::move(name)) {}

  std::string toString(size_t lvl) const override { return name; }

  std::string name;
};

struct Terminator : Statement {
  Terminator(StatementKind kind) : Statement(kind) {}
};

struct Chunk : Statement {
  Chunk() : Statement(SK_Chunk) {}
  Chunk(std::deque<std::unique_ptr<Statement>> stmts)
      : Statement(SK_Chunk), stmts(std::move(stmts)) {}
  Chunk(std::deque<std::unique_ptr<Statement>> stmts,
        std::unique_ptr<Terminator> terminator)
      : Statement(SK_Chunk),
        stmts(std::move(stmts)),
        terminator(std::move(terminator)) {}

  std::string toString(size_t lvl) const override {
    std::string res;
    for (auto &stmt : stmts) {
      tab(res, lvl);
      res += stmt->toString(lvl);
      res += '\n';
    }
    if (terminator) {
      tab(res, lvl);
      res += terminator->toString(lvl);
      res += '\n';
    }
    return res;
  }

  std::deque<std::unique_ptr<Statement>> stmts;
  std::unique_ptr<Terminator> terminator;
};

struct DoStatement : Statement {
  DoStatement() : Statement(SK_DoStatement) {}
  DoStatement(std::unique_ptr<Chunk> chunk)
      : Statement(SK_DoStatement), chunk(std::move(chunk)) {}

  std::string toString(size_t lvl) const override {
    std::string res{"do\n"};
    res += chunk->toString(lvl + 1);
    tab(res, lvl);
    res += "end";
    return res;
  }

  std::unique_ptr<Chunk> chunk;
};

struct WhileStatement : Statement {
  WhileStatement() : Statement(SK_WhileStatement) {}
  WhileStatement(std::unique_ptr<Expression> cond, std::unique_ptr<Chunk> body)
      : Statement(SK_WhileStatement),
        cond(std::move(cond)),
        body(std::move(body)) {}

  std::string toString(size_t lvl) const override {
    std::string res{"while ("};
    res += cond->toString(lvl);
    res += ") do\n";
    res += body->toString(lvl + 1);
    tab(res, lvl);
    res += "end";
    return res;
  }

  std::unique_ptr<Expression> cond;
  std::unique_ptr<Chunk> body;
};

struct RepeatStatement : Statement {
  RepeatStatement() : Statement(SK_RepeatStatement) {}
  RepeatStatement(std::unique_ptr<Expression> cond, std::unique_ptr<Chunk> body)
      : Statement(SK_RepeatStatement),
        cond(std::move(cond)),
        body(std::move(body)) {}

  std::string toString(size_t lvl) const override {
    std::string res{"repeat\n"};
    res += body->toString(lvl + 1);
    tab(res, lvl);
    res += "until (";
    res += cond->toString(lvl);
    res += ")";
    return res;
  }

  std::unique_ptr<Expression> cond;
  std::unique_ptr<Chunk> body;
};

struct ReturnStatement : Terminator {
  ReturnStatement() : Terminator(SK_ReturnStatement) {}
  ReturnStatement(std::deque<std::unique_ptr<Expression>> expList)
      : Terminator(SK_ReturnStatement), expList(std::move(expList)) {}

  std::string toString(size_t lvl) const override {
    std::string res{"return"};
    if (!expList.empty()) res += " ";
    for (auto &exp : expList) {
      if (res.back() != ' ') res += ", ";
      res += exp->toString(lvl);
    }
    return res;
  }

  std::deque<std::unique_ptr<Expression>> expList;
};

struct BreakStatement : Terminator {
  BreakStatement() : Terminator(SK_BreakStatement) {}

  std::string toString(size_t lvl) const override { return "break"; }
};

struct IfStatement : Statement {
  IfStatement() : Statement(SK_IfStatement) {}

  std::string toString(size_t lvl) const override {
    std::string res{"if ("};
    res += cond->toString(lvl);
    res += ") then\n";
    res += consequence->toString(lvl + 1);
    for (auto &[cond, alter] : alternatives) {
      tab(res, lvl);
      res += "elseif (";
      res += cond->toString(lvl);
      res += ") then\n";
      res += alter->toString(lvl + 1);
    }
    if (otherwise) {
      tab(res, lvl);
      res += "else\n";
      res += otherwise->toString(lvl + 1);
    }
    tab(res, lvl);
    res += "end";
    return res;
  }

  std::unique_ptr<Expression> cond;
  std::unique_ptr<Chunk> consequence;
  std::deque<std::pair<std::unique_ptr<Expression>, std::unique_ptr<Chunk>>>
      alternatives;
  std::unique_ptr<Chunk> otherwise;
};

struct ForStatement : Statement {
  ForStatement() : Statement(SK_ForStatement) {}

  std::string toString(size_t lvl) const override {
    std::string res{"for "};
    res += var->toString(lvl);
    res += " = ";
    res += first->toString(lvl);
    res += ", ";
    res += last->toString(lvl);
    if (step) {
      res += ", ";
      res += step->toString(lvl);
    }
    res += " do\n";
    res += body->toString(lvl + 1);
    tab(res, lvl);
    res += "end";
    return res;
  }

  std::unique_ptr<Reference> var;
  std::unique_ptr<Expression> first;
  std::unique_ptr<Expression> last;
  std::unique_ptr<Expression> step;
  std::unique_ptr<Chunk> body;
};

struct ForeachStatement : Statement {
  ForeachStatement() : Statement(SK_ForeachStatement) {}

  std::string toString(size_t lvl) const override {
    std::string res{"for "};
    for (auto &name : nameList) {
      if (res.back() != ' ') res += ", ";
      res += name->toString(lvl);
    }
    res += " in ";
    for (auto &exp : expList) {
      if (res.back() != ' ') res += ", ";
      res += exp->toString(lvl);
    }
    res += " do\n";
    res += body->toString(lvl + 1);
    tab(res, lvl);
    res += "end";
    return res;
  }

  std::deque<std::unique_ptr<Reference>> nameList;
  std::deque<std::unique_ptr<Expression>> expList;
  std::unique_ptr<Chunk> body;
};

struct Assignment : Statement {
  Assignment() : Statement(SK_AssignmentStatement) {}
  Assignment(std::deque<std::unique_ptr<Expression>> leftSide,
             std::deque<std::unique_ptr<Expression>> rightSide,
             bool isLocal = false)
      : Statement(SK_AssignmentStatement),
        leftSide(std::move(leftSide)),
        rightSide(std::move(rightSide)),
        isLocal(isLocal) {}

  std::string toString(size_t lvl) const override {
    std::string res;
    if (isLocal) res = "local ";
    bool first = true;
    for (auto &ptr : leftSide) {
      if (!first) res += ", ";
      first = false;
      res += ptr->toString(lvl);
    }
    if (!rightSide.empty()) res += " = ";
    first = true;
    for (auto &ptr : rightSide) {
      if (!first) res += ", ";
      first = false;
      res += ptr->toString(lvl);
    }
    return res;
  }

  bool isLocal = false;
  std::deque<std::unique_ptr<Expression>> leftSide;
  std::deque<std::unique_ptr<Expression>> rightSide;
};

struct Literal : Expression {
  Literal() : Expression(SK_Literal) {}
  Literal(StatementKind kind) : Expression(kind) {}
};

struct Number : Literal {
  Number(double value) : Literal(SK_Number), value(value) {}

  std::string toString(size_t lvl) const override {
    return std::to_string(value);
  }

  double value;
};

struct String : Literal {
  String(std::string value) : Literal(SK_String), value(value) {}

  std::string toString(size_t lvl) const override {
    return std::string{'"'} + value + '"';
  }

  std::string value;
};

struct Bool : Literal {
  Bool(bool value) : Literal(SK_Bool), value(value) {}

  std::string toString(size_t lvl) const override {
    return value ? "true" : "false";
  }

  bool value;
};

struct Nil : Literal {
  Nil() : Literal(SK_Nil) {}

  std::string toString(size_t lvl) const override { return "nil"; }
};

struct Etc : Literal {
  Etc() : Literal(SK_Etc) {}

  std::string toString(size_t lvl) const override { return "..."; }
};

struct UnaryOperator : Expression {
  UnaryOperator(UnaryOperatorKind kind, std::unique_ptr<Expression> sub)
      : Expression(SK_UnaryOperator), kind(kind), sub(std::move(sub)) {}

  std::string toString(size_t lvl) const override {
    std::string res{'('};
    switch (kind) {
      case UO_LNot:
        res += "not ";
        break;
      case UO_Negate:
        res += "-";
        break;
      case UO_SizeOf:
        res += "#";
        break;
    }
    res += sub->toString(lvl);
    res += ")";
    return res;
  }

  UnaryOperatorKind kind;
  std::unique_ptr<Expression> sub;
};

struct BinaryOperator : Expression {
  BinaryOperator(BinaryOperatorKind kind, std::unique_ptr<Expression> left,
                 std::unique_ptr<Expression> right)
      : Expression(SK_UnaryOperator),
        kind(kind),
        left(std::move(left)),
        right(std::move(right)) {}

  std::string toString(size_t lvl) const override {
    if (kind == BO_Access) {
      std::string res{left->toString(lvl)};
      res += '[';
      res += right->toString(lvl);
      res += ']';
      return res;
    } else if (kind == BO_FieldAccess) {
      std::string res{left->toString(lvl)};
      res += '.';
      res += right->toString(lvl);
      return res;
    } else if (kind == BO_SelfAccess) {
      std::string res{left->toString(lvl)};
      res += ':';
      res += right->toString(lvl);
      return res;
    }
    std::string res{'('};
    res += left->toString(lvl);
    res += " ";
    switch (kind) {
      case BO_Access:
      case BO_FieldAccess:
      case BO_SelfAccess:
        break;
      case BO_Add:
        res += "+";
        break;
      case BO_Subtract:
        res += "-";
        break;
      case BO_Multiply:
        res += "*";
        break;
      case BO_Divide:
        res += "/";
        break;
      case BO_Power:
        res += "^";
        break;
      case BO_Modulo:
        res += "%";
        break;
      case BO_Concat:
        res += "..";
        break;
      case BO_LessThan:
        res += "<";
        break;
      case BO_LessThanOrEqualsTo:
        res += "<=";
        break;
      case BO_GreaterThan:
        res += ">";
        break;
      case BO_GreaterThanOrEqualsTo:
        res += ">=";
        break;
      case BO_EqualsTo:
        res += "==";
        break;
      case BO_NotEqualsTo:
        res += "~=";
        break;
      case BO_LAnd:
        res += "and";
        break;
      case BO_LOr:
        res += "or";
        break;
    }
    res += " ";
    res += right->toString(lvl);
    res += ")";
    return res;
  }

  BinaryOperatorKind kind;
  std::unique_ptr<Expression> left;
  std::unique_ptr<Expression> right;
};

struct Call : Expression {
  Call() : Expression(SK_Call) {}

  std::string toString(size_t lvl) const override {
    std::string res{callee->toString(lvl)};
    res += '(';
    bool first = true;
    for (auto &arg : args) {
      if (!first) res += ", ";
      first = false;
      res += arg->toString(lvl);
    }
    res += ')';
    return res;
  }

  std::unique_ptr<Expression> callee;
  std::vector<std::unique_ptr<Expression>> args;
};

struct Function : Expression {
  Function() : Expression(SK_Function) {}
  std::string toString(size_t lvl) const override {
    std::string res;
    if (isLocal) res = "local ";
    res += "function ";
    if (name) res += name->toString(lvl);
    res += "(";
    for (auto &param : params) {
      if (res.back() != '(') res += ", ";
      res += param->toString(lvl);
    }
    // if (isVariadic) res += "...";
    res += ")\n";
    res += body->toString(lvl + 1);
    tab(res, lvl);
    res += "end";
    return res;
  }

  bool isLocal = false;
  std::unique_ptr<Expression> name;
  std::deque<std::unique_ptr<Expression>> params;
  bool isVariadic = false;
  std::unique_ptr<Chunk> body;
};

}  // namespace lua
}  // namespace combinatorial_parsers

#endif
