#include <iostream>
#include <functional>
#include <set>
#include "closure.hpp"
#include "Permutation.hpp"
using namespace std;

const int n = 5;

int main(void) {
 int cycle[n];
 for (int i=0; i<n; i++) cycle[i] = i+1;
 Permutation start[2] = {
  Permutation::fromCycle(cycle, cycle+n),
  Permutation::transposition(1,2)
 };
 const set<Permutation> sn = closure2A<Permutation>(multiplies<Permutation>(), start+0, start+2);
 set<Permutation>::const_iterator iter;
 for (iter = sn.begin(); iter != sn.end(); iter++)
  cout << iter->lehmer() << '\t' << string(*iter) << endl;
 return 0;
}
