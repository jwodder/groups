#include <iostream>
#include "Permutation.hpp"
using namespace std;

int main(void) {
 Permutation begin = Permutation::identity();
 Permutation end = Permutation::firstOfDegree(6);
 Permutation p;
 for (p = begin; p < end; p++)
  cout << p.lehmer() << '\t' << string(p) << endl;
 cout << "---" << endl;
 p = end;
 for (p--; p >= begin; p--) {
  cout << p.lehmer() << '\t' << string(p) << endl;
  if (!p) break;
 }
 return 0;
}
