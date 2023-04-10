#include <type_traits>

struct ABase {};

template <typename T>
concept A = std::is_base_of<ABase, T>::value;

template <template <A> typename f>
struct B {
  using type = int;
};

template <template <typename> typename f>
struct С {
  using type = typename B<f>::type;
};

int main() {}