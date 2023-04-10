#ifndef TUPLE_HPP_
#define TUPLE_HPP_

#include <type_traits>

struct Tuple2Base {};

template <typename T>
concept Tuple2 = std::is_base_of<Tuple2Base, T>::value;

template <typename fst, typename snd>
struct Tuple2Ctor : Tuple2Base {};

template <Tuple2 t>
struct Fst {};

template <typename fst, typename snd>
struct Fst<Tuple2Ctor<fst, snd>> {
  using result = fst;
};

template <Tuple2 t>
struct Snd {};

template <typename fst, typename snd>
struct Snd<Tuple2Ctor<fst, snd>> {
  using result = snd;
};

#endif