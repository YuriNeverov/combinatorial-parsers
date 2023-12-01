#include "lua-aux.h"
#include "lua.h"
#include "parsers.h"

#include "STLFunctionExtras.h"

#include <cctype>
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include <type_traits>

#define WORD(word) pSeqWord<sizeof(word) - 1>(word)

using namespace combinatorial_parsers;

template <typename T>
struct Print {
  static std::string from(const lua::Statement *arg) {
    return arg->toString(0);
  }
};

template <int N>
struct Print<const char[N]> {
  static std::string from(const char arg[N]) { return arg; }
};

template <typename... Ts>
struct Print<std::tuple<Ts...>> {
  static std::string fromImpl(const Ts &...args) {
    std::string s;
    ((s += Print<Ts>::from(args), s += ", "), ...);
    return s;
  }
  static std::string from(const std::tuple<Ts...> &tuple) {
    std::string s{"tuple{"};
    s += applyFun(fromImpl, tuple);
    s += "}";
    return s;
  }
};

template <>
struct Print<char> {
  static std::string from(char arg) { return {arg}; }
};

template <>
struct Print<int> {
  static std::string from(int arg) { return std::to_string(arg); }
};

template <>
struct Print<std::string_view> {
  static std::string from(std::string_view arg) { return std::string(arg); }
};

template <>
struct Print<std::string> {
  static std::string from(std::string_view arg) { return std::string(arg); }
};

template <>
struct Print<const char *> {
  static std::string from(const char *arg) { return arg; }
};

template <typename T>
struct Print<std::vector<T>> {
  static std::string from(const std::vector<T> &arg) {
    if (arg.empty()) return "vec{}";
    std::string s{"vec{"};
    s += Print<T>::from(arg[0]);
    for (int i = 1; i < arg.size(); i++) {
      s += ", ";
      s += Print<T>::from(arg[i]);
    }
    s += "}";
    return s;
  }
};

template <typename T>
struct Print<std::deque<T>> {
  static std::string from(const std::deque<T> &arg) {
    if (arg.empty()) return "deque{}";
    std::string s{"deque{"};
    s += Print<T>::from(arg[0]);
    for (int i = 1; i < arg.size(); i++) {
      s += ", ";
      s += Print<T>::from(arg[i]);
    }
    s += "}";
    return s;
  }
};

template <typename T>
struct Print<std::deque<std::unique_ptr<T>>> {
  static std::string from(const std::deque<std::unique_ptr<T>> &arg) {
    if (arg.empty()) return "deque{}";
    std::string s{"deque{"};
    s += Print<T>::from(arg[0].get());
    for (int i = 1; i < arg.size(); i++) {
      s += ", ";
      s += Print<T>::from(arg[i].get());
    }
    s += "}";
    return s;
  }
};

template <typename T>
void printResult(const Result<T> &res) {
  if (!res.value) {
    std::cerr << "<nothing>";
    return;
  }
  std::cerr << "(value: " << Print<std::remove_reference_t<T>>::from(*res.value)
            << ", "
            << "tail: '" << res.tail << "')";
}

template <typename T>
void printResult(const Result<std::unique_ptr<T>> &res) {
  if (!res.value) {
    std::cerr << "<nothing>";
    return;
  }
  std::cerr << "(value: "
            << Print<std::remove_reference_t<T>>::from(res.value->get()) << ", "
            << "tail: '" << res.tail << "')";
}

template <typename T>
void print(T &&res) {
  std::cerr << Print<std::remove_reference_t<T>>::from(res) << "\n";
}

template <typename Parser, typename... Args>
void applyParsers(Parser parser, Args &&...args) {
  ((std::cerr << args << " ->\n    ", printResult(parser(args)),
    std::cerr << "\n\n"),
   ...);
}

template <typename Arg>
void test(Arg arg) {
  std::cerr << arg << " ";
}

template <typename Arg, typename... Args>
void test(Arg arg, Args... args) {
  std::cerr << arg << " ";
  test(args...);
}

template <typename T>
using ptr = std::unique_ptr<T>;

void luaDemo() {
  const auto identStart = [](unsigned char ch) -> bool {
    if (ch == '_') return true;
    return std::isalpha(ch);
  };
  const auto identAfter = [](unsigned char ch) -> bool {
    if (ch == '_') return true;
    return std::isalnum(ch);
  };
  const auto isDigit = [](unsigned char ch) -> bool {
    return std::isdigit(ch);
  };
  const auto isSpace = [](unsigned char ch) -> bool {
    return std::isspace(ch);
  };

  // -> string
  const auto pIdentAfter = _cChar(identAfter);
  const auto parserName = pSeqF(
      [](char first, std::deque<char> rest) {
        std::string s{first};
        for (char ch : rest) s += ch;
        return s;
      },
      _cChar(identStart), pStar(pIdentAfter));

  std::unordered_set<char> firstForNum{'1', '2', '3', '4', '5',
                                       '6', '7', '8', '9'};
  const auto digit = _cChar(isDigit);
  const auto space = _cChar(isSpace);
  const auto spaces = pStar(space);

  const auto dot = pChar1('.');
  const auto semicolon = pChar1(':');
  const auto colon = pChar1(';');
  const auto comma = pChar1(',');
  const auto eq = pChar1('=');
  const auto minus = pChar1('-');
  const auto plus = pChar1('+');
  const auto mul = pChar1('*');
  const auto div = pChar1('/');
  const auto hat = pChar1('^');
  const auto mod = pChar1('%');
  const auto lbrace = pChar1('(');
  const auto rbrace = pChar1(')');
  const auto lcurly = pChar1('{');
  const auto rcurly = pChar1('}');
  const auto lsquare = pChar1('[');
  const auto rsquare = pChar1(']');
  const auto hash = pChar1('#');
  const auto tail = pChar1('~');

  const auto quote = pChar1('"');
  const auto notQuote = pCharNot({'"'});

  const auto parserNil = WORD("nil");
  const auto parserTrue = WORD("true");
  const auto parserFalse = WORD("false");

  const auto parserNot = WORD("not");
  const auto parserAnd = WORD("and");
  const auto parserOr = WORD("or");
  const auto parserCat = WORD("..");
  const auto parserEtc = WORD("...");
  const auto parserLt = WORD("<");
  const auto parserGt = WORD(">");
  const auto parserLtEq = WORD("<=");
  const auto parserGtEq = WORD(">=");
  const auto parserEqEq = WORD("==");
  const auto parserNEq = WORD("~=");
  const auto parserHash = WORD("#");
  const auto parserNegate = WORD("-");

  const auto parserDo = WORD("do");
  const auto parserEnd = WORD("end");
  const auto parserFor = WORD("for");
  const auto parserIn = WORD("in");
  const auto parserWhile = WORD("while");
  const auto parserRepeat = WORD("repeat");
  const auto parserUntil = WORD("until");
  const auto parserReturn = WORD("return");
  const auto parserBreak = WORD("break");
  const auto parserIf = WORD("if");
  const auto parserElseif = WORD("elseif");
  const auto parserElse = WORD("else");
  const auto parserThen = WORD("then");
  const auto parserLocal = WORD("local");
  const auto parserFunctionW = WORD("function");

  const auto &parserPrior8 = parserOr;
  const auto &parserPrior7 = parserAnd;
  const auto parserPrior6 =
      pOr(parserLt, parserGt, parserLtEq, parserGtEq, parserEqEq, parserNEq);
  const auto &parserPrior5 = parserCat;
  const auto parserPrior4 = pOr(minus, plus);
  const auto parserPrior3 = pOr(mul, div, mod);
  const auto parserUnOp = pOr(parserNot, parserHash, parserNegate);
  const auto &parserPrior1 = hat;

  const auto parserNumber = pOr(
      pSeqF(
          [](std::string pref, std::deque<char> digits) {
            if (digits.empty()) pref += '0';
            for (auto ch : digits) pref += ch;
            return pref;
          },
          pSeqWord<2>("0."), pStar(digit)),
      pSeqF(
          [](char first, std::deque<char> digits,
             std::optional<std::deque<char>> tail) {
            std::string s{first};
            for (auto ch : digits) s += ch;
            if (tail) {
              s += '.';
              for (auto ch : *tail) s += ch;
            }
            return s;
          },
          pChar(firstForNum), pStar(digit), pOpt(pSeqN<1>(dot, pStar(digit)))));

  const auto parserStrLiteral = pSeqFNs<1>(
      [](std::deque<char> deq) {
        std::string s;
        for (char ch : deq) s += ch;
        return s;
      },
      quote, pStar(notQuote), quote);

  std::function<Result<ptr<lua::Expression>>(std::string_view)> parserExpImpl;
  std::function<Result<ptr<lua::Expression>>(std::string_view)>
      parserFunctionImpl;
  llvm::function_ref<Result<ptr<lua::Expression>>(std::string_view)> parserExp =
      parserExpImpl;
  llvm::function_ref<Result<ptr<lua::Expression>>(std::string_view)>
      parserFunction = parserFunctionImpl;

  const std::function<Result<ptr<lua::Expression>>(std::string_view)>
      parserLiteral =
          pOr(pMap(
                  [](std::string) -> ptr<lua::Expression> {
                    return std::make_unique<lua::Nil>();
                  },
                  parserNil),
              pMap(
                  [](std::string) -> ptr<lua::Expression> {
                    return std::make_unique<lua::Bool>(true);
                  },
                  parserTrue),
              pMap(
                  [](std::string) -> ptr<lua::Expression> {
                    return std::make_unique<lua::Bool>(false);
                  },
                  parserFalse),
              pMap(
                  [](std::string num) -> ptr<lua::Expression> {
                    return std::make_unique<lua::Number>(std::stold(num));
                  },
                  parserNumber),
              pMap(
                  [](std::string str) -> ptr<lua::Expression> {
                    return std::make_unique<lua::String>(std::move(str));
                  },
                  parserStrLiteral),
              pMap(
                  [](std::string str) -> ptr<lua::Expression> {
                    return std::make_unique<lua::Etc>();
                  },
                  parserEtc));

  const auto parserCallee =
      pOr(pSeqN<2>(lbrace, spaces, parserExp, spaces, rbrace),
          pMap(
              [](std::string name) -> ptr<lua::Expression> {
                return std::make_unique<lua::Reference>(std::move(name));
              },
              parserName));

  const auto parserSepExp = pSeqN<2>(comma, spaces, parserExp);
  const auto parserExpList = pSeqFNs<0, 2>(
      [](ptr<lua::Expression> exp, std::deque<ptr<lua::Expression>> deq) {
        deq.push_front(std::move(exp));
        return std::move(deq);
      },
      parserExp, spaces, pStar(parserSepExp));

  const auto parserArgs = pSeqFNs<2>(
      [](std::optional<std::deque<ptr<lua::Expression>>> deq) {
        if (!deq) return std::deque<ptr<lua::Expression>>();
        return std::move(*deq);
      },
      lbrace, spaces, pOpt(parserExpList), spaces, rbrace);

  const auto parserArgsRTE = pMap(
      [](std::deque<ptr<lua::Expression>> deq) -> ptr<lua_aux::RightTailExp> {
        return std::make_unique<lua_aux::Args>(std::move(deq));
      },
      parserArgs);
  const auto parserSelfArgsRTE = pSeqFNs<2, 4>(
      [](std::string name,
         std::deque<ptr<lua::Expression>> deq) -> ptr<lua_aux::RightTailExp> {
        return std::make_unique<lua_aux::SelfArgs>(std::move(name),
                                                   std::move(deq));
      },
      colon, spaces, parserName, spaces, parserArgs);
  const auto parserAccessRTE = pSeqFNs<2>(
      [](ptr<lua::Expression> exp) -> ptr<lua_aux::RightTailExp> {
        return std::make_unique<lua_aux::Access>(std::move(exp));
      },
      lsquare, spaces, parserExp, spaces, rsquare);
  const auto parserFieldRTE = pSeqFNs<2>(
      [](std::string name) -> ptr<lua_aux::RightTailExp> {
        return std::make_unique<lua_aux::Field>(std::move(name));
      },
      dot, spaces, parserName);
  const auto parserRTE = pSeqN<1>(spaces, pOr(parserArgsRTE, parserSelfArgsRTE,
                                              parserAccessRTE, parserFieldRTE));
  const auto parserVarRTE =
      pSeqN<1>(spaces, pOr(parserAccessRTE, parserFieldRTE));
  const auto parserCallRTE =
      pSeqN<1>(spaces, pOr(parserArgsRTE, parserSelfArgsRTE));

  using ExpPair = std::pair<lua::BinaryOperatorKind, ptr<lua::Expression>>;
  const auto parserAtom =
      pOr(parserLiteral, parserFunction,
          pSeqFNs<0, 1>(
              [](ptr<lua::Expression> left,
                 std::deque<ptr<lua_aux::RightTailExp>> deq) {
                for (auto &rte : deq) left = rte->make(std::move(left));
                return left;
              },
              parserCallee, pStar(parserRTE)));
  const auto parserP1Tail = pSeqFNs<1, 3>(
      [](char ch, ptr<lua::Expression> exp) -> ExpPair {
        return std::make_pair(lua::BO_Power, std::move(exp));
      },
      spaces, parserPrior1, spaces, parserAtom);
  const auto parserExp1 = pSeqFNs<0, 1>(
      [](ptr<lua::Expression> left, std::deque<ExpPair> deq) {
        deq.emplace_front(lua::BO_Power, std::move(left));
        ptr<lua::Expression> right = std::move(deq.back().second);
        for (int it = deq.size() - 2; it >= 0; it--)
          right = std::make_unique<lua::BinaryOperator>(
              lua::BO_Power, std::move(deq[it].second), std::move(right));
        return right;
      },
      parserAtom, pStar(parserP1Tail));
  const auto parserP2Head = pSeqN<0>(parserUnOp, spaces);
  const auto parserExp2 = pSeqFNs<0, 1>(
      [](std::deque<std::string> unops, ptr<lua::Expression> exp) {
        for (int it = unops.size() - 1; it >= 0; it--) {
          lua::UnaryOperatorKind uok;
          if (unops[it] == "not")
            uok = lua::UO_LNot;
          else if (unops[it] == "#")
            uok = lua::UO_SizeOf;
          else
            uok = lua::UO_Negate;
          exp = std::make_unique<lua::UnaryOperator>(uok, std::move(exp));
        }
        return exp;
      },
      pStar(parserP2Head), parserExp1);
  const auto parserP3Tail = pSeqFNs<1, 3>(
      [](char ch, ptr<lua::Expression> exp) -> ExpPair {
        lua::BinaryOperatorKind bok;
        if (ch == '*')
          bok = lua::BO_Multiply;
        else if (ch == '/')
          bok = lua::BO_Divide;
        else
          bok = lua::BO_Modulo;
        return std::make_pair(bok, std::move(exp));
      },
      spaces, parserPrior3, spaces, parserExp2);
  const auto parserExp3 = pSeqFNs<0, 1>(
      [](ptr<lua::Expression> left, std::deque<ExpPair> deq) {
        for (auto &pair : deq)
          left = std::make_unique<lua::BinaryOperator>(
              pair.first, std::move(left), std::move(pair.second));
        return left;
      },
      parserExp2, pStar(parserP3Tail));
  const auto parserP4Tail = pSeqFNs<1, 3>(
      [](char ch, ptr<lua::Expression> exp) -> ExpPair {
        lua::BinaryOperatorKind bok;
        if (ch == '+')
          bok = lua::BO_Add;
        else
          bok = lua::BO_Subtract;
        return std::make_pair(bok, std::move(exp));
      },
      spaces, parserPrior4, spaces, parserExp3);
  const auto parserExp4 = pSeqFNs<0, 1>(
      [](ptr<lua::Expression> left, std::deque<ExpPair> deq) {
        for (auto &pair : deq)
          left = std::make_unique<lua::BinaryOperator>(
              pair.first, std::move(left), std::move(pair.second));
        return left;
      },
      parserExp3, pStar(parserP4Tail));
  const auto parserP5Tail = pSeqFNs<1, 3>(
      [](std::string, ptr<lua::Expression> exp) -> ExpPair {
        return std::make_pair(lua::BO_Concat, std::move(exp));
      },
      spaces, parserPrior5, spaces, parserExp4);
  const auto parserExp5 = pSeqFNs<0, 1>(
      [](ptr<lua::Expression> left, std::deque<ExpPair> deq) {
        deq.emplace_front(lua::BO_Concat, std::move(left));
        ptr<lua::Expression> right = std::move(deq.back().second);
        for (int it = deq.size() - 2; it >= 0; it--)
          right = std::make_unique<lua::BinaryOperator>(
              lua::BO_Concat, std::move(deq[it].second), std::move(right));
        return right;
      },
      parserExp4, pStar(parserP5Tail));
  const auto parserP6Tail = pSeqFNs<1, 3>(
      [](std::string str, ptr<lua::Expression> exp) -> ExpPair {
        lua::BinaryOperatorKind bok;
        if (str == "<")
          bok = lua::BO_LessThan;
        else if (str == ">")
          bok = lua::BO_GreaterThan;
        else if (str == "<=")
          bok = lua::BO_LessThanOrEqualsTo;
        else if (str == ">=")
          bok = lua::BO_GreaterThanOrEqualsTo;
        else if (str == "~=")
          bok = lua::BO_NotEqualsTo;
        else
          bok = lua::BO_EqualsTo;
        return std::make_pair(bok, std::move(exp));
      },
      spaces, parserPrior6, spaces, parserExp5);
  const auto parserExp6 = pSeqFNs<0, 1>(
      [](ptr<lua::Expression> left, std::deque<ExpPair> deq) {
        for (auto &pair : deq)
          left = std::make_unique<lua::BinaryOperator>(
              pair.first, std::move(left), std::move(pair.second));
        return left;
      },
      parserExp5, pStar(parserP6Tail));
  const auto parserP7Tail = pSeqFNs<1, 3>(
      [](std::string str, ptr<lua::Expression> exp) -> ExpPair {
        return std::make_pair(lua::BO_LAnd, std::move(exp));
      },
      spaces, parserPrior7, spaces, parserExp6);
  const auto parserExp7 = pSeqFNs<0, 1>(
      [](ptr<lua::Expression> left, std::deque<ExpPair> deq) {
        for (auto &pair : deq)
          left = std::make_unique<lua::BinaryOperator>(
              pair.first, std::move(left), std::move(pair.second));
        return left;
      },
      parserExp6, pStar(parserP7Tail));
  const auto parserP8Tail = pSeqFNs<1, 3>(
      [](std::string str, ptr<lua::Expression> exp) -> ExpPair {
        return std::make_pair(lua::BO_LOr, std::move(exp));
      },
      spaces, parserPrior8, spaces, parserExp7);
  const auto parserExp8 = pSeqFNs<0, 1>(
      [](ptr<lua::Expression> left, std::deque<ExpPair> deq) {
        for (auto &pair : deq)
          left = std::make_unique<lua::BinaryOperator>(
              pair.first, std::move(left), std::move(pair.second));
        return left;
      },
      parserExp7, pStar(parserP8Tail));

  parserExpImpl = parserExp8;

  const auto parserRTEVarSeq = pSeqF(
      [](std::deque<ptr<lua_aux::RightTailExp>> deq,
         ptr<lua_aux::RightTailExp> last) {
        deq.push_back(std::move(last));
        return deq;
      },
      pStar(parserCallRTE), parserVarRTE);
  const auto parserVar =
      pOr(pSeqF(
              [](std::string name,
                 std::deque<std::deque<ptr<lua_aux::RightTailExp>>> last) {
                ptr<lua::Expression> left =
                    std::make_unique<lua::Reference>(std::move(name));
                for (auto &deq : last)
                  for (auto &rte : deq) left = rte->make(std::move(left));
                return left;
              },
              parserName, pStar(parserRTEVarSeq)),
          pSeqFNs<2, 5, 6>(
              [](ptr<lua::Expression> left,
                 std::deque<ptr<lua_aux::RightTailExp>> deq,
                 std::deque<std::deque<ptr<lua_aux::RightTailExp>>> last) {
                last.push_front(std::move(deq));
                for (auto &deq : last)
                  for (auto &rte : deq) left = rte->make(std::move(left));
                return left;
              },
              lbrace, spaces, parserExp, spaces, rbrace, parserRTEVarSeq,
              pStar(parserRTEVarSeq)));

  const auto parserFunctionCall = pSeqF(
      [](ptr<lua::Expression> callee, ptr<lua_aux::RightTailExp> args,
         std::deque<ptr<lua_aux::RightTailExp>> deq) -> ptr<lua::Statement> {
        deq.push_front(std::move(args));
        for (auto &rte : deq) callee = rte->make(std::move(callee));
        return callee;
      },
      pOr(parserVar, pSeqN<2>(lbrace, spaces, parserExp, spaces, rbrace)),
      parserCallRTE, pStar(parserCallRTE));

  const auto parserSepVar = pSeqN<3>(spaces, comma, spaces, parserVar);
  const auto parserVarList = pSeqF(
      [](ptr<lua::Expression> left, std::deque<ptr<lua::Expression>> deq) {
        deq.push_front(std::move(left));
        return deq;
      },
      parserVar, pStar(parserSepVar));

  const auto parserSepName = pSeqN<3>(spaces, comma, spaces, parserName);
  const auto parserNameList = pSeqF(
      [](std::string name, std::deque<std::string> tail) {
        tail.push_front(std::move(name));
        return tail;
      },
      parserName, pStar(parserSepName));

  const auto parserParList =
      pOr(pSeqF(
              [](std::deque<std::string> deq, auto opt) {
                std::deque<ptr<lua::Expression>> res;
                for (auto &ref : deq)
                  res.emplace_back(
                      std::make_unique<lua::Reference>(std::move(ref)));
                if (opt) res.emplace_back(std::make_unique<lua::Etc>());
                return res;
              },
              parserNameList, pOpt(pSeq(spaces, comma, spaces, parserEtc))),
          pMap(
              [](std::string) -> std::deque<ptr<lua::Expression>> {
                std::deque<ptr<lua::Expression>> res;
                res.emplace_back(std::make_unique<lua::Etc>());
                return res;
              },
              parserEtc));

  const auto dotName = pSeqN<3>(spaces, dot, spaces, parserName);
  const auto parserFuncName = pSeqF(
      [](std::string first, std::deque<std::string> parts,
         std::optional<std::string> tail) {
        ptr<lua::Expression> left =
            std::make_unique<lua::Reference>(std::move(first));
        for (auto &part : parts)
          left = std::make_unique<lua::BinaryOperator>(
              lua::BO_FieldAccess, std::move(left),
              std::make_unique<lua::Reference>(std::move(part)));
        if (tail)
          left = std::make_unique<lua::BinaryOperator>(
              lua::BO_SelfAccess, std::move(left),
              std::make_unique<lua::Reference>(std::move(*tail)));
        return left;
      },
      parserName, pStar(dotName),
      pOpt(pSeqN<3>(spaces, semicolon, spaces, parserName)));

  using FuncBody = std::pair<std::deque<ptr<lua::Expression>>, ptr<lua::Chunk>>;

  std::function<Result<ptr<lua::Chunk>>(std::string_view)> parserChunk;
  llvm::function_ref<Result<ptr<lua::Chunk>>(std::string_view)> parserBlock =
      parserChunk;

  const auto parserFuncBody = pSeqFNs<2, 6>(
      [](std::optional<std::deque<ptr<lua::Expression>>> params,
         ptr<lua::Chunk> body) -> FuncBody {
        return std::make_pair(
            params ? std::move(*params) : std::deque<ptr<lua::Expression>>{},
            std::move(body));
      },
      lbrace, spaces, pOpt(parserParList), spaces, rbrace, spaces, parserBlock,
      spaces, parserEnd);

  parserFunctionImpl = pSeqFNs<2>(
      [](FuncBody body) -> ptr<lua::Expression> {
        auto func = std::make_unique<lua::Function>();
        func->params = std::move(body.first);
        if (!func->params.empty() && func->params.back()->kind == lua::SK_Etc)
          func->isVariadic = true;
        func->body = std::move(body.second);
        return func;
      },
      parserFunctionW, spaces, parserFuncBody);

  using IfAlternative = std::pair<ptr<lua::Expression>, ptr<lua::Chunk>>;
  const auto parserIfAlternative = pSeqFNs<3, 7>(
      [](ptr<lua::Expression> cond, ptr<lua::Chunk> body) -> IfAlternative {
        return std::make_pair(std::move(cond), std::move(body));
      },
      spaces, parserElseif, spaces, parserExp, spaces, parserThen, spaces,
      parserBlock);

  const auto parserStat = pOr(
      pSeqFNs<0, 4>(
          [](std::deque<ptr<lua::Expression>> varList,
             std::deque<ptr<lua::Expression>> expList) -> ptr<lua::Statement> {
            return std::make_unique<lua::Assignment>(std::move(varList),
                                                     std::move(expList));
          },
          parserVarList, spaces, eq, spaces, parserExpList),
      parserFunctionCall,
      pSeqFNs<2>(
          [](ptr<lua::Chunk> chunk) -> ptr<lua::Statement> {
            return std::make_unique<lua::DoStatement>(std::move(chunk));
          },
          parserDo, spaces, parserBlock, spaces, parserEnd),
      pSeqFNs<2, 6>(
          [](ptr<lua::Expression> cond,
             ptr<lua::Chunk> body) -> ptr<lua::Statement> {
            return std::make_unique<lua::WhileStatement>(std::move(cond),
                                                         std::move(body));
          },
          parserWhile, spaces, parserExp, spaces, parserDo, spaces, parserBlock,
          spaces, parserEnd),
      pSeqFNs<2, 6>(
          [](ptr<lua::Chunk> body,
             ptr<lua::Expression> cond) -> ptr<lua::Statement> {
            return std::make_unique<lua::RepeatStatement>(std::move(cond),
                                                          std::move(body));
          },
          parserRepeat, spaces, parserBlock, spaces, parserUntil, spaces,
          parserExp),
      pSeqFNs<2, 6, 7, 8>(
          [](ptr<lua::Expression> cond, ptr<lua::Chunk> body,
             std::deque<IfAlternative> alternatives,
             std ::optional<ptr<lua::Chunk>> otherwise) -> ptr<lua::Statement> {
            auto ifStmt = std::make_unique<lua::IfStatement>();
            ifStmt->cond = std::move(cond);
            ifStmt->consequence = std::move(body);
            ifStmt->alternatives = std::move(alternatives);
            if (otherwise) ifStmt->otherwise = std::move(*otherwise);
            return ifStmt;
          },
          parserIf, spaces, parserExp, spaces, parserThen, spaces, parserBlock,
          pStar(parserIfAlternative),
          pOpt(pSeqN<3>(spaces, parserElse, spaces, parserBlock)), spaces,
          parserEnd),
      pSeqFNs<2, 6, 10, 11, 15>(
          [](std::string name, ptr<lua::Expression> first,
             ptr<lua::Expression> last,
             std::optional<ptr<lua::Expression>> step,
             ptr<lua::Chunk> body) -> ptr<lua::Statement> {
            auto forStmt = std::make_unique<lua::ForStatement>();
            forStmt->var = std::make_unique<lua::Reference>(std::move(name));
            forStmt->first = std::move(first);
            forStmt->last = std::move(last);
            if (step) forStmt->step = std::move(*step);
            forStmt->body = std::move(body);
            return forStmt;
          },
          parserFor, spaces, parserName, spaces, eq, spaces, parserExp, spaces,
          comma, spaces, parserExp,
          pOpt(pSeqN<3>(spaces, comma, spaces, parserExp)), spaces, parserDo,
          spaces, parserBlock, spaces, parserEnd),
      pSeqFNs<2, 6, 10>(
          [](std::deque<std::string> nameList,
             std::deque<ptr<lua::Expression>> expList,
             ptr<lua::Chunk> body) -> ptr<lua::Statement> {
            auto foreachStmt = std::make_unique<lua::ForeachStatement>();
            for (auto &name : nameList)
              foreachStmt->nameList.emplace_back(
                  std::make_unique<lua::Reference>(std::move(name)));
            foreachStmt->expList = std::move(expList);
            foreachStmt->body = std::move(body);
            return foreachStmt;
          },
          parserFor, spaces, parserNameList, spaces, parserIn, spaces,
          parserExpList, spaces, parserDo, spaces, parserBlock, spaces,
          parserEnd),
      pSeqFNs<2, 4>(
          [](ptr<lua::Expression> funcName,
             FuncBody body) -> ptr<lua::Statement> {
            auto func = std::make_unique<lua::Function>();
            func->name = std::move(funcName);
            func->params = std::move(body.first);
            if (!func->params.empty() &&
                func->params.back()->kind == lua::SK_Etc)
              func->isVariadic = true;
            func->body = std::move(body.second);
            return func;
          },
          parserFunctionW, spaces, parserFuncName, spaces, parserFuncBody),
      pSeqFNs<4, 6>(
          [](std::string name, FuncBody body) -> ptr<lua::Statement> {
            auto func = std::make_unique<lua::Function>();
            func->isLocal = true;
            func->name = std::make_unique<lua::Reference>(std::move(name));
            func->params = std::move(body.first);
            if (!func->params.empty() &&
                func->params.back()->kind == lua::SK_Etc)
              func->isVariadic = true;
            func->body = std::move(body.second);
            return func;
          },
          parserLocal, spaces, parserFunctionW, spaces, parserName, spaces,
          parserFuncBody),
      pSeqFNs<2, 3>(
          [](std::deque<std::string> nameList,
             std::optional<std::deque<ptr<lua::Expression>>> expList)
              -> ptr<lua::Statement> {
            auto assign = std::make_unique<lua::Assignment>();
            assign->isLocal = true;
            for (auto &name : nameList)
              assign->leftSide.emplace_back(
                  std::make_unique<lua::Reference>(std::move(name)));
            if (expList) assign->rightSide = std::move(*expList);
            return assign;
          },
          parserLocal, spaces, parserNameList,
          pOpt(pSeqN<3>(spaces, eq, spaces, parserExpList))));

  const auto parserStatTail = pSeqN<1>(spaces, parserStat, spaces, pOpt(colon));
  parserChunk = pSeqF(
      [](std::deque<ptr<lua::Statement>> deq,
         std::optional<ptr<lua::Terminator>> term) {
        if (!term) return std::make_unique<lua::Chunk>(std::move(deq));
        return std::make_unique<lua::Chunk>(std::move(deq), std::move(*term));
      },
      pStar(parserStatTail),
      pOpt(pSeqN<1>(
          spaces,
          pOr(pSeqFNs<0, 2>(
                  [](std::string,
                     std::optional<std::deque<ptr<lua::Expression>>> expList)
                      -> ptr<lua::Terminator> {
                    if (!expList)
                      return std::make_unique<lua::ReturnStatement>();
                    return std::make_unique<lua::ReturnStatement>(
                        std::move(*expList));
                  },
                  parserReturn, spaces, pOpt(parserExpList)),
              pMap(
                  [](std::string) -> ptr<lua::Terminator> {
                    return std::make_unique<lua::BreakStatement>();
                  },
                  parserBreak)))));
  applyParsers(parserExp, "nil", "true", "false~", "10.123~",
               "\"hello world this is string literal!\"", "lala~", "lala [10]~",
               "lala [lala]. field [10] (10,   \"Hello\")~", "10 ^ 10",
               "10 ^ exp",
               "lala [lala]. field [10] (10,   \"Hello\") ^ 10 ^ hello~",
               "- not # 10 ^ 10", "- not # 10 ^ 10 * 10 ^ (- 15)",
               "10 * 10 * 20 / 30 + 10 + 20", "10 + 80 * 20 + 10 * 20 / 10 + 2",
               "10 + 80 * 20 + 10 * 20 / 10 + 2 .. 10 * 10 * 20 / 30 + 10 + 20 "
               ".. \"hello\" .. identifier .. fun(args)",
               "10 * 10 - 5 and 7 or 9 * 10 .. ident ^ a ^b > 10 * 2");
  applyParsers(parserBlock, "var = 1", "f(a, b, c) b(a) (c + d)",
               "do var1 = 1 ; f() do print(helllo, ada, ...) end end",
               "while 10 + 2^2 do print(hello) repeat print(a, b, c) return "
               "10, 20, 30 until false break end",
               "var = function (a, b, c, ...) print (a) end",
               "local function Hello (a, b, c, ...) print (a) end",
               "function Hello.Fi:Ha (a, b, c, ...) print (a) end",
               "local name", "local name, var, b = 10, 20 and 220, ...",
               "for i = 10, 20, 1 do print(i) break end",
               "if true then print (hello, world) elseif 10 + 5 == 10 then do "
               "do end end else print(\"Hi\") end");
}

int main() {
  luaDemo();

  std::cerr << "End of parsing!\n";
  return 0;
}
