#pragma once
//
//#include <tuple>
//using namespace std;

// needs:
// tuple constructors (almost all)
//   - note: size of tuples must be same anyways
// make_tuple
// get
//   - number get -- all kind of references supported
//   - type get -- all kind of references supported + compilation error on many types
//     example:
//       tuple<int, double, int> x;
//       get<int>(x); -- compilation error
//       get<0>(x); -- ok
// compare operators

// helpers to implement:
// tuple_element & tuple_element_t -- type helper, gets type of element by it's number
// tuple_size & tuple_size_v -- gets tuple size

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
