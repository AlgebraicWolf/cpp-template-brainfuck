#ifndef FUNCTION_LIST_HPP_
#define FUNCTION_LIST_HPP_

#include <type_traits>

struct FuncListBase {};

template <typename T>
concept FuncList = std::is_base_of<FuncListBase, T>::value;

struct FuncNil : FuncListBase {};

template <template <typename> typename f, FuncList fs> 
struct FuncCons : FuncListBase {};

#endif