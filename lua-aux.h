#ifndef COMBINATORIAL_PARSERS_LUA_AUX_H_
#define COMBINATORIAL_PARSERS_LUA_AUX_H_

#include "lua.h"

#include <deque>
#include <memory>

namespace combinatorial_parsers {
namespace lua_aux {

enum RightTailExpKind {
  RTEK_Args,
  RTEK_SelfArgs,
  RTEK_Access,
  RTEK_Field,
};

struct RightTailExp {
  RightTailExp(RightTailExpKind kind) : kind(kind) {}

  RightTailExpKind kind;

  virtual std::unique_ptr<lua::Expression> make(
      std::unique_ptr<lua::Expression>) = 0;

  virtual ~RightTailExp() = default;
};

struct Args : RightTailExp {
  Args() : RightTailExp(RTEK_Args) {}
  Args(std::deque<std::unique_ptr<lua::Expression>> args)
      : RightTailExp(RTEK_Args), args(std::move(args)) {}

  std::unique_ptr<lua::Expression> make(
      std::unique_ptr<lua::Expression> left) override {
    auto call = std::make_unique<lua::Call>();
    call->callee = std::move(left);
    for (auto &arg : args) call->args.emplace_back(std::move(arg));
    return call;
  }
  std::deque<std::unique_ptr<lua::Expression>> args;
};

struct SelfArgs : RightTailExp {
  SelfArgs(std::string name, std::deque<std::unique_ptr<lua::Expression>> args)
      : RightTailExp(RTEK_SelfArgs),
        name(std::move(name)),
        args(std::move(args)) {}

  std::unique_ptr<lua::Expression> make(
      std::unique_ptr<lua::Expression> left) override {
    auto call = std::make_unique<lua::Call>();
    call->callee = std::make_unique<lua::BinaryOperator>(
        lua::BO_SelfAccess, std::move(left),
        std::make_unique<lua::Reference>(std::move(name)));
    for (auto &arg : args) call->args.emplace_back(std::move(arg));
    return call;
  }
  std::string name;
  std::deque<std::unique_ptr<lua::Expression>> args;
};

struct Access : RightTailExp {
  Access(std::unique_ptr<lua::Expression> exp)
      : RightTailExp(RTEK_Access), exp(std::move(exp)) {}

  std::unique_ptr<lua::Expression> make(
      std::unique_ptr<lua::Expression> left) override {
    return std::make_unique<lua::BinaryOperator>(
        lua::BO_Access, std::move(left), std::move(exp));
  }
  std::unique_ptr<lua::Expression> exp;
};

struct Field : RightTailExp {
  Field(std::string name) : RightTailExp(RTEK_Field), name(std::move(name)) {}

  std::unique_ptr<lua::Expression> make(
      std::unique_ptr<lua::Expression> left) override {
    return std::make_unique<lua::BinaryOperator>(
        lua::BO_FieldAccess, std::move(left),
        std::make_unique<lua::Reference>(std::move(name)));
  }
  std::string name;
};

}  // namespace lua_aux
}  // namespace combinatorial_parsers

#endif
