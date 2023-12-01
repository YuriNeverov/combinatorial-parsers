#ifndef COMBINATORIAL_PARSERS_PARSERS_H_
#define COMBINATORIAL_PARSERS_PARSERS_H_

#include <deque>
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_set>
#include <utility>
#include <vector>

namespace combinatorial_parsers {

template <typename T>
struct Result {
  using type = T;
  std::optional<T> value = std::nullopt;
  std::string_view tail = "";
};

template <typename Type, typename Tuple, size_t... I>
auto addElementImpl(Tuple &&tuple, Type &&element, std::index_sequence<I...>) {
  return std::make_tuple(std::move(std::get<I>(tuple))...,
                         std::forward<Type>(element));
}

template <typename Type, typename... Types>
std::tuple<Types..., Type> addElement(std::tuple<Types...> &&tuple,
                                      Type &&element) {
  auto index =
      std::make_index_sequence<std::tuple_size_v<std::tuple<Types...>>>();
  return addElementImpl(std::move(tuple), std::move(element), index);
}

template <typename F, typename Tuple, size_t... I>
auto applyFunImpl(F &&fun, Tuple &&tuple, std::index_sequence<I...>) {
  return fun(std::get<I>(std::forward<Tuple>(tuple))...);
}

template <typename F, typename Tuple>
auto applyFun(F &&fun, Tuple &&tuple) {
  auto index = std::make_index_sequence<
      std::tuple_size_v<std::remove_reference_t<Tuple>>>();
  return applyFunImpl(std::forward<F>(fun), std::forward<Tuple>(tuple), index);
}

template <typename Parser>
using ParserResult =
    typename std::invoke_result_t<Parser, std::string_view>::type;

template <typename... Parsers>
using SeqResult = std::tuple_element_t<0, std::tuple<Parsers...>>;

template <typename T>
auto _cEmpty() {
  return [](std::string_view text) -> Result<T> {
    return Result<T>{T{}, text};
  };
}

template <typename Predicate>
auto _cChar(Predicate &&pred) {
  return [pred = std::move(pred)](std::string_view text) {
    if (text.empty() || !pred(text[0])) return Result<char>();
    return Result<char>{text[0], text.substr(1)};
  };
}

template <typename Combinator, typename LeftParser, typename RightParser>
auto _cCombine(Combinator &&combinator, LeftParser &&left,
               RightParser &&right) {
  return [combinator = std::forward<Combinator>(combinator),
          left = std::forward<LeftParser>(left),
          right = std::forward<RightParser>(right)](std::string_view text) {
    using T = std::invoke_result_t<Combinator, ParserResult<LeftParser>,
                                   ParserResult<RightParser>>;

    auto resLeft = left(text);
    if (!resLeft.value) return Result<T>();
    auto resRight = right(resLeft.tail);
    if (!resRight.value) return Result<T>();
    return Result<T>{
        combinator(std::move(*resLeft.value), std::move(*resRight.value)),
        resRight.tail};
  };
}

template <typename Parser, typename AlterParser>
auto _cEither(Parser &&parser, AlterParser &&alterParser) {
  return [parser = std::move(parser),
          alterParser = std::move(alterParser)](std::string_view text) {
    auto res = parser(text);
    if (res.value) return res;
    return alterParser(text);
  };
}

template <typename Parser, typename Predicate>
auto _cFailIf(Parser &&parser, Predicate &&predicate) {
  return [parser = std::move(parser),
          predicate = std::move(predicate)](std::string_view text) {
    auto res = parser(text);
    if (!res.value || predicate(*res.value))
      return Result<decltype(*res.value)>();
    return res;
  };
}

template <typename F, typename Parser>
auto pMap(F &&fun, Parser &&parser) {
  return [fun = std::move(fun),
          parser = std::move(parser)](std::string_view text) {
    using T = std::invoke_result_t<F, ParserResult<Parser>>;

    auto res = parser(text);
    if (res.value) return Result<T>{fun(std::move(*res.value)), res.tail};
    return Result<T>();
  };
}

template <typename T>
auto pEmpty = _cEmpty<T>;

inline auto pChar(std::unordered_set<char> &&chars) {
  return _cChar(
      [chars = std::move(chars)](char ch) { return chars.count(ch); });
}

inline auto pChar(const std::unordered_set<char> &chars) {
  return _cChar([&chars](char ch) { return chars.count(ch); });
}

inline auto pChar1(char ch) {
  return _cChar([ch](char c) { return c == ch; });
}

inline auto pCharNot(std::unordered_set<char> &&chars) {
  return _cChar(
      [chars = std::move(chars)](char ch) { return !chars.count(ch); });
}

template <typename Parser, typename T>
inline auto pIgnore(Parser &&parser, T constant) {
  return pMap([constant = std::move(constant)](auto &&_) { return constant; },
              std::forward<Parser>(parser));
}

template <typename T>
auto pFailIf = _cFailIf<T>;

template <typename T>
auto vecCombinator(std::vector<T> acc, T el) {
  acc.push_back(el);
  return acc;
}

template <typename T, typename ParserAcc, typename ParserEl>
auto pSeqVImpl(ParserAcc &&parserAcc, ParserEl &&parserEl) {
  return _cCombine(vecCombinator<T>, std::forward<ParserAcc>(parserAcc),
                   std::forward<ParserEl>(parserEl));
}

template <typename T, typename ParserAcc, typename ParserEl,
          typename... Parsers>
auto pSeqVImpl(ParserAcc &&parserAcc, ParserEl &&parserEl,
               Parsers &&...parsers) {
  return pSeqVImpl<T>(
      _cCombine(vecCombinator<T>, std::forward<ParserAcc>(parserAcc),
                std::forward<ParserEl>(parserEl)),
      std::forward<Parsers>(parsers)...);
}

template <typename... Parsers>
auto pSeqV(Parsers &&...parsers) {
  using T = ParserResult<SeqResult<Parsers...>>;
  return pSeqVImpl<T>(pEmpty<std::vector<T>>(),
                      std::forward<Parsers>(parsers)...);
}

template <typename F, typename... Parsers>
auto pSeqVF(F &&fun, Parsers &&...parsers) {
  return pMap(
      [fun = std::forward<F>(fun)](auto vec) { return fun(std::move(vec)); },
      pSeqV(std::forward<Parsers>(parsers)...));
}

template <typename ParserAcc, typename ParserEl>
auto pSeqImpl(ParserAcc &&parserAcc, ParserEl &&parserEl) {
  return _cCombine(
      [](auto acc, auto el) {
        return addElement(std::move(acc), std::move(el));
      },
      std::forward<ParserAcc>(parserAcc), std::forward<ParserEl>(parserEl));
}

template <typename ParserAcc, typename ParserEl, typename... Parsers>
auto pSeqImpl(ParserAcc &&parserAcc, ParserEl &&parserEl,
              Parsers &&...parsers) {
  return pSeqImpl(
      _cCombine(
          [](auto acc, auto el) {
            return addElement(std::move(acc), std::move(el));
          },
          std::forward<ParserAcc>(parserAcc), std::forward<ParserEl>(parserEl)),
      std::forward<Parsers>(parsers)...);
}

template <typename... Parsers>
auto pSeq(Parsers &&...parsers) {
  return pSeqImpl(pEmpty<std::tuple<>>(), std::forward<Parsers>(parsers)...);
}

template <typename F, typename... Parsers>
auto pSeqF(F &&fun, Parsers &&...parsers) {
  return pMap([fun = std::forward<F>(fun)](
                  auto tuple) { return applyFun(fun, std::move(tuple)); },
              pSeq(std::forward<Parsers>(parsers)...));
}

template <size_t N, typename... Parsers>
auto pSeqN(Parsers &&...parsers) {
  return pMap([](auto tuple) { return std::move(std::get<N>(tuple)); },
              pSeq(std::forward<Parsers>(parsers)...));
}

template <size_t... N, typename... Parsers>
auto pSeqNs(Parsers &&...parsers) {
  return pMap(
      [](auto &&tuple) {
        return std::make_tuple(std::move(std::get<N>(tuple))...);
      },
      pSeq(std::forward<Parsers>(parsers)...));
}

template <size_t... N, typename F, typename... Parsers>
auto pSeqFNs(F &&fun, Parsers &&...parsers) {
  return pMap(
      [fun = std::forward<F>(fun)](auto &&tuple) {
        return fun(std::move(std::get<N>(tuple))...);
      },
      pSeq(std::forward<Parsers>(parsers)...));
}

template <typename ParserOnly>
auto pOrImpl(ParserOnly &&parserOnly) {
  return parserOnly;
}

template <typename ParserLeft, typename ParserRight>
auto pOrImpl(ParserLeft &&parserLeft, ParserRight &&parserRight) {
  return _cEither(std::forward<ParserLeft>(parserLeft),
                  std::forward<ParserRight>(parserRight));
}

template <typename ParserLeft, typename ParserRight, typename... Parsers>
auto pOrImpl(ParserLeft &&parserLeft, ParserRight &&parserRight,
             Parsers &&...parsers) {
  return pOrImpl(_cEither(std::forward<ParserLeft>(parserLeft),
                          std::forward<ParserRight>(parserRight)),
                 std::forward<Parsers>(parsers)...);
}

template <typename FirstParser, typename... Parsers>
auto pOr(FirstParser &&firstParser, Parsers &&...parsers) {
  return pOrImpl(std::forward<FirstParser>(firstParser),
                 std::forward<Parsers>(parsers)...);
}

template <typename Parser>
auto pOpt(Parser &&parser) {
  using T = ParserResult<Parser>;
  return pOr(pMap([](T a) -> std::optional<T> { return a; },
                  std::forward<Parser>(parser)),
             pEmpty<std::optional<T>>());
}

// Do NOT use this with temporary parser!
template <typename Parser>
std::function<Result<std::deque<ParserResult<Parser>>>(std::string_view)> pStar(
    const Parser &parser) {
  using T = ParserResult<Parser>;
  return pOr(
      _cCombine(
          [](auto el, std::deque<T> deq) {
            deq.push_front(std::move(el));
            return deq;
          },
          parser,
          [&parser](std::string_view text) { return pStar(parser)(text); }),
      pEmpty<std::deque<T>>());
}

template <size_t... I>
auto pSeqWordImpl(const char *seq, std::index_sequence<I...>) {
  return pMap(
      [](std::vector<char> v) {
        std::string str;
        for (char ch : v) str += ch;
        return str;
      },
      pSeqV(pChar({seq[I]})...));
}

template <size_t N>
auto pSeqWord(const char seq[N]) {
  return pSeqWordImpl(seq, std::make_index_sequence<N>());
}

}  // namespace combinatorial_parsers

#endif
