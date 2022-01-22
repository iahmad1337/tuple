#pragma once

#include "meta_utils.h"
#include <type_traits>

namespace detail {

/******************************************************************************
 *                           Tuple storage                                    *
 ******************************************************************************/

template <typename... Types>
struct tuple_storage;

template <typename T, typename... Rest>
struct tuple_storage<T, Rest...> {
  template <bool default_ctor = std::is_default_constructible_v<T>,
            typename = std::enable_if_t<default_ctor>>
  tuple_storage() : val(), rest() {}

  explicit tuple_storage(const T& _val, const Rest&... _rest)
      : val(_val), rest(_rest...) {}
  tuple_storage(const tuple_storage& other)
      : val(other.val), rest(other.rest) {}
  tuple_storage(tuple_storage&& other)
      : val(std::move(other.val)), rest(std::move(other.rest)) {}

  tuple_storage& operator=(const tuple_storage& other) = default;

  tuple_storage& operator=(tuple_storage&& other) = default;

  template <std::size_t... Is, typename... Ts>
  tuple_storage(std::index_sequence<Is...>, const tuple_storage<Ts...>& other)
      : tuple_storage(other.template get_helper<Is>()...) {}

  template <std::size_t... Is, typename... Ts>
  tuple_storage(std::index_sequence<Is...>, tuple_storage<Ts...>&& other)
      : tuple_storage(std::move(other.template get_helper<Is>())...) {}

  template <typename UType, typename... UTypes,
            typename = std::enable_if_t<
                std::is_constructible_v<T, UType> &&
                std::is_constructible_v<tuple_storage<Rest...>, UTypes...>>>
  tuple_storage(UType&& first_arg, UTypes&&... args)
      : val(std::forward<UType>(first_arg)),
        rest(std::forward<UTypes>(args)...) {}

  void swap(tuple_storage& other) {
    using std::swap;
    swap(val, other.val);
    if constexpr (sizeof...(Rest) > 0) {
      rest.swap(other.rest);
    }
  }

  ~tuple_storage() = default;

  template <std::size_t I>
  meta::ith_t<I, T, Rest...>& get_helper() {
    if constexpr (I == 0) {
      return val;
    } else {
      return rest.template get_helper<I - 1>();
    }
  }

  template <std::size_t I>
  meta::ith_t<I, T, Rest...> const& get_helper() const {
    if constexpr (I == 0) {
      return val;
    } else {
      return rest.template get_helper<I - 1>();
    }
  }

protected:
  T val;
  tuple_storage<Rest...> rest;
};

template <>
struct tuple_storage<> {};

/******************************************************************************
 *                                Enablers                                   *
 ******************************************************************************/

template <bool copyable>
struct enable_copy {
  enable_copy() = default;
  enable_copy(const enable_copy&) = delete;
  enable_copy& operator=(const enable_copy&) = delete;
  enable_copy(enable_copy&&) = default;
  enable_copy& operator=(enable_copy&&) = default;
};

template <>
struct enable_copy<true> {
  enable_copy() = default;
  enable_copy(const enable_copy&) = default;
  enable_copy& operator=(const enable_copy&) = default;
  enable_copy(enable_copy&&) = default;
  enable_copy& operator=(enable_copy&&) = default;
};

template <bool movable>
struct enable_move {
  enable_move() = default;
  enable_move(const enable_move&) = default;
  enable_move& operator=(const enable_move&) = default;
  enable_move(enable_move&&) = delete;
  enable_move& operator=(enable_move&&) = delete;
};

template <>
struct enable_move<true> {
  enable_move() = default;
  enable_move(const enable_move&) = default;
  enable_move& operator=(const enable_move&) = default;
  enable_move(enable_move&&) = default;
  enable_move& operator=(enable_move&&) = default;
};

} // namespace detail

/******************************************************************************
 *                                The tuple                                   *
 ******************************************************************************/

template <typename... Types>
struct tuple;

template <>
struct tuple<> {};

template <typename... Types>
struct tuple : detail::tuple_storage<Types...>,
               detail::enable_copy<
                   std::conjunction_v<std::is_copy_constructible<Types>...>>,
               detail::enable_move<
                   std::conjunction_v<std::is_move_constructible<Types>...>> {
  using base = detail::tuple_storage<Types...>;
  // same types constructors
  // note: think *when* you need to enable all of them
  template <bool default_ctor = (std::is_default_constructible_v<Types> && ...),
            typename = std::enable_if_t<default_ctor>>
  constexpr tuple() : detail::tuple_storage<Types...>() {}
  template <bool direct_ctor =
                std::conjunction_v<std::is_copy_constructible<Types>...>,
            typename = std::enable_if_t<direct_ctor>>
  constexpr tuple(const Types&... args) : base(args...) {}

  tuple(const tuple& other) = default;
  tuple(tuple&& other) = default;
  tuple& operator=(const tuple& other) = default;
  tuple& operator=(tuple&& other) = default;

  // no need for conditional explicitness
  // convert types constructors
  template <typename... UTypes,
            typename = std::enable_if_t<
                sizeof...(Types) == sizeof...(UTypes) &&
                (std::is_constructible_v<Types, UTypes> && ...)>>
  constexpr tuple(UTypes&&... args) : base(std::forward<UTypes>(args)...) {}

  template <typename... UTypes,
            typename = std::enable_if_t<
                sizeof...(Types) == sizeof...(UTypes) &&
                (std::is_constructible_v<Types, const UTypes&> && ...)>>
  constexpr tuple(const tuple<UTypes...>& other)
      : base(std::index_sequence_for<UTypes...>{},
             static_cast<typename tuple<UTypes...>::base const&>(other)) {}
  template <typename... UTypes,
            typename = std::enable_if_t<
                sizeof...(Types) == sizeof...(UTypes) &&
                (std::is_constructible_v<Types, UTypes> && ...)>>
  constexpr tuple(tuple<UTypes...>&& other)
      : base(std::index_sequence_for<UTypes...>{},
             static_cast<typename tuple<UTypes...>::base&&>(std::move(other))) {
  }

  template <std::size_t N>
  constexpr meta::ith_t<N, Types...>& get() & noexcept {
    return this->template get_helper<N>();
  }

  template <std::size_t N>
  constexpr meta::ith_t<N, Types...>&& get() && noexcept {
    return std::move(this->template get_helper<N>());
  }

  template <std::size_t N>
  constexpr const meta::ith_t<N, Types...>& get() const& noexcept {
    return this->template get_helper<N>();
  }

  template <std::size_t N>
  constexpr const meta::ith_t<N, Types...>&& get() const&& noexcept {
    return std::move(this->template get_helper<N>());
  }

  void swap(tuple& other) {
    base::swap(static_cast<base&>(other));
  }
};

/******************************************************************************
 *                                Helper classes                              *
 ******************************************************************************/

// make_tuple -- constructor with auto deducing types
// note references are dereferenced
// see cppreference
template <class T>
struct unwrap_refwrapper {
  using type = T;
};

template <class T>
struct unwrap_refwrapper<std::reference_wrapper<T>> {
  using type = T&;
};

template <class T>
using unwrap_decay_t =
    typename unwrap_refwrapper<typename std::decay<T>::type>::type;

template <typename... Types>
constexpr tuple<unwrap_decay_t<Types>...> make_tuple(Types&&... args) {
  return tuple<unwrap_decay_t<Types>...>(std::forward<Types>(args)...);
}

// NOTE: if not tuple passed to helpers -- must be compiler error
// tuple_element -- return type by it's number (type field)
template <std::size_t N, typename... Types>
struct tuple_element;

// recursive case
template <std::size_t I, class Head, class... Tail>
struct tuple_element<I, tuple<Head, Tail...>>
    : tuple_element<I - 1, tuple<Tail...>> {};

// base case
template <class Head, class... Tail>
struct tuple_element<0, tuple<Head, Tail...>> {
  using type = Head;
};

// pls implement tuple_element_t
template <std::size_t I, typename Tuple>
using tuple_element_t = typename tuple_element<I, Tuple>::type;

// tuple_size -- value equals number of elements (you can use
// std::integral_constant)
template <typename... Types>
struct tuple_size;

template <typename... Types>
struct tuple_size<tuple<Types...>>
    : std::integral_constant<std::size_t, sizeof...(Types)> {};

// pls implement tuple_size_v
template <typename Tuple>
constexpr inline std::size_t tuple_size_v = tuple_size<Tuple>::value;

/******************************************************************************
 *                                `get` overloads                             *
 ******************************************************************************/

// get -- main function to interact with tuple
// NOTE: think how to replace int in return type
// NOTE: compiler error if N > tuple_size
template <std::size_t N, typename... Types>
constexpr std::enable_if_t<N < sizeof...(Types), meta::ith_t<N, Types...>>& get(tuple<Types...>& t) noexcept {
  return t.template get<N>();
}

template <std::size_t N, typename... Types>
constexpr std::enable_if_t<N < sizeof...(Types), meta::ith_t<N, Types...>>&& get(tuple<Types...>&& t) noexcept {
  return std::move(t).template get<N>();
}

template <std::size_t N, typename... Types>
constexpr const std::enable_if_t<N < sizeof...(Types), meta::ith_t<N, Types...>>&
get(const tuple<Types...>& t) noexcept {
  return t.template get<N>();
}

template <std::size_t N, typename... Types>
constexpr const std::enable_if_t<N < sizeof...(Types), meta::ith_t<N, Types...>>&&
get(const tuple<Types...>&& t) noexcept {
  return std::move(t).template get<N>();
}

// gets by type
// NOTE: compiler error if many T in one tuple
template <typename T, typename... Types>
constexpr std::enable_if_t<meta::once<T, Types...>, T>&
get(tuple<Types...>& t) noexcept {
  return get<meta::idx_v<T, Types...>>(t);
}

template <typename T, typename... Types>
constexpr std::enable_if_t<meta::once<T, Types...>, T>&&
get(tuple<Types...>&& t) noexcept {
  return get<meta::idx_v<T, Types...>>(std::move(t));
}

template <typename T, typename... Types>
constexpr const std::enable_if_t<meta::once<T, Types...>, T>&
get(const tuple<Types...>& t) noexcept {
  return get<meta::idx_v<T, Types...>>(t);
}

template <typename T, typename... Types>
constexpr const std::enable_if_t<meta::once<T, Types...>, T>&&
get(const tuple<Types...>&& t) noexcept {
  return get<meta::idx_v<T, Types...>>(std::move(t));
}

/******************************************************************************
 *                     swap & relational operators                            *
 ******************************************************************************/

// swap specialization
template <typename... Types>
void swap(tuple<Types...>& first, tuple<Types...>& second) {
  first.swap(second);
}

namespace detail {
constexpr inline int LESS = -1;
constexpr inline int EQUAL = 0;
constexpr inline int GREATER = 1;
} // namespace detail

template <std::size_t I = 0, typename... TTypes, typename... UTypes>
int lex_comp(const tuple<TTypes...>& first, const tuple<UTypes...>& second) {
  if constexpr (I + 1 == sizeof...(TTypes)) {
    return detail::EQUAL;
  } else {
    if (get<I>(first) < get<I>(second)) {
      return detail::LESS;
    } else if (get<I>(first) > get<I>(second)) {
      return detail::GREATER;
    } else {
      return lex_comp<I + 1>(first, second);
    }
  }
}

// compare operators
template <typename... TTypes, typename... UTypes>
constexpr bool operator==(const tuple<TTypes...>& first,
                          const tuple<UTypes...>& second) {
  static_assert(sizeof...(TTypes) == sizeof...(UTypes));
  return lex_comp(first, second) == detail::EQUAL;
}

template <typename... TTypes, typename... UTypes>
constexpr bool operator!=(const tuple<TTypes...>& first,
                          const tuple<UTypes...>& second) {
  static_assert(sizeof...(TTypes) == sizeof...(UTypes));
  return lex_comp(first, second) != detail::EQUAL;
}

template <typename... TTypes, typename... UTypes>
constexpr bool operator<(const tuple<TTypes...>& first,
                         const tuple<UTypes...>& second) {
  static_assert(sizeof...(TTypes) == sizeof...(UTypes));
  return lex_comp(first, second) == detail::LESS;
}

template <typename... TTypes, typename... UTypes>
constexpr bool operator>(const tuple<TTypes...>& first,
                         const tuple<UTypes...>& second) {
  static_assert(sizeof...(TTypes) == sizeof...(UTypes));
  return lex_comp(first, second) == detail::GREATER;
}

template <typename... TTypes, typename... UTypes>
constexpr bool operator<=(const tuple<TTypes...>& first,
                          const tuple<UTypes...>& second) {
  static_assert(sizeof...(TTypes) == sizeof...(UTypes));
  return !(first > second);
}

template <typename... TTypes, typename... UTypes>
constexpr bool operator>=(const tuple<TTypes...>& first,
                          const tuple<UTypes...>& second) {
  static_assert(sizeof...(TTypes) == sizeof...(UTypes));
  return !(first < second);
}
