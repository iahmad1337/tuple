#pragma once

//#include <tuple>
//using namespace std;

template<typename... Types>
struct tuple {
  // same types constructors
  constexpr tuple();
  constexpr tuple(const Types&... args);

  tuple(const tuple& other);
  tuple(tuple&& other);

  // no need for conditional explicitness
  // convert types constructors
  template<typename... UTypes>
  constexpr tuple(UTypes&&... args);

  template<typename... UTypes>
  constexpr tuple(const tuple<UTypes...>& other);
  template<typename... UTypes>
  constexpr tuple(tuple<UTypes...>&& other);
};

// make_tuple -- constructor with auto deducing types
// note references are dereferenced
template<typename... Types>
constexpr tuple<VTypes...> make_tuple(Types&&... args);

// NOTE: if not tuple passed to helpers -- must be compiler error
// tuple_element -- return type by it's number (type field)
template<std::size_t N, typename... Types>
struct tuple_element;

// pls implement tuple_element_t

// tuple_size -- value equals number of elements (you can use std::integral_constant)
template<typename... Types>
struct tuple_size;

// pls implement tuple_size_v

// get -- main function to interact with tuple
// NOTE: think how to replace int in return type
// NOTE: compiler error if N > tuple_size
template<std::size_t N, typename... Types>
constexpr int& get(tuple<Types...>& t) noexcept;

template<std::size_t N, typename... Types>
constexpr int&& get(tuple<Types...>&& t) noexcept;

template<std::size_t N, typename... Types>
constexpr const int& get(const tuple<Types...>& t) noexcept;

template<std::size_t N, typename... Types>
constexpr const int&& get(const tuple<Types...>&& t) noexcept;


// gets by type
// NOTE: compiler error if many T in one tuple
template<typename T, typename... Types>
constexpr T& get(tuple<Types...>& t) noexcept;

template<typename T, typename... Types>
constexpr T&& get(tuple<Types...>&& t) noexcept;

template<typename T, typename... Types>
constexpr const T& get(const tuple<Types...>& t) noexcept;

template<typename T, typename... Types>
constexpr const T&& get(const tuple<Types...>&& t) noexcept;

// swap specialization
template<typename... TTypes, typename... UTypes>
void swap(tuple<TTypes...>& first, tuple<UTypes...>& second);

// compare operators
template<typename... TTypes, typename... UTypes>
constexpr bool operator==(const tuple<TTypes...>& first, const tuple<UTypes...>& second);
template<typename... TTypes, typename... UTypes>
constexpr bool operator!=(const tuple<TTypes...>& first, const tuple<UTypes...>& second);
template<typename... TTypes, typename... UTypes>
constexpr bool operator<(const tuple<TTypes...>& first, const tuple<UTypes...>& second);
template<typename... TTypes, typename... UTypes>
constexpr bool operator>(const tuple<TTypes...>& first, const tuple<UTypes...>& second);
template<typename... TTypes, typename... UTypes>
constexpr bool operator<=(const tuple<TTypes...>& first, const tuple<UTypes...>& second);
template<typename... TTypes, typename... UTypes>
constexpr bool operator>=(const tuple<TTypes...>& first, const tuple<UTypes...>& second);
