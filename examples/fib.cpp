#include <functional>
#include <iostream>
#include <string>
using namespace std;
int main() {
  std::function<int(int)> fib = [=, &fib](int n) {
    if (n <= 2) {
      return 1;
    }

    return fib(n - 1) + fib(n - 2);
  };

  int i = 1;
  int j = 30;
  while (i <= j) {
    cout << "";
    cout << i;
    cout << "";
    cout << fib(i) << endl;
    i = i + 1;
  }
}
