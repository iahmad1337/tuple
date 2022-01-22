#pragma once

#include <type_traits>

namespace meta {

template <std::size_t Index, typename First, typename... Rest>
struct ith {
  using type = typename ith<Index - 1, Rest...>::type;
};

template <typename First, typename... Rest>
struct ith<0, First, Rest...> {
  using type = First;
};

template <std::size_t Index, typename Pivot, typename First, typename... Rest>
struct idx {
  constexpr static std::size_t value = idx<Index + 1, Pivot, Rest...>::value;
};

template <std::size_t Index, typename Pivot, typename... Types>
struct idx<Index, Pivot, Pivot, Types...> {
  constexpr static std::size_t value = Index;
};

template <typename Pivot, typename First, typename... Rest>
constexpr inline std::size_t idx_v = idx<0, Pivot, First, Rest...>::value;

template <std::size_t Index, typename... Types>
using ith_t = typename ith<Index, Types...>::type;

template <typename Pivot, typename... Types>
inline constexpr bool once = ((std::is_same_v<Pivot, Types> ? 1:0) + ...) == 1;

} // namespace meta

