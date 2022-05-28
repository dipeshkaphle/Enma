#include <functional>
#include <iostream>
#include <string>
using namespace std;
int main() {
  std::function<int(int)> sum = [=, &sum](int n) {
    int i = 0;
    int sm = 0;
    while (i <= n) {
      sm = sm + i;
      i = i + 1;
    }

    return sm;
  };

  std::function<int(int)> fib = [=, &fib](int n) {
    if (n <= 1) {
      return n;
    }

    return fib(n - 1) + fib(n - 2);
  };

  cout << "";
  cout << sum(1) << endl;
  cout << "";
  cout << sum(4) << endl;
  int j = 10;
  int y = 1;
  while (y <= j) {
    cout << "";
    cout << y;
    cout << "";
    cout << fib(y) << endl;
    y = y + 1;
  }
}
