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

  cout << fib(30) << endl;
}
