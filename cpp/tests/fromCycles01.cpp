#include <iostream>
#include <vector>
#include "Permutation.hpp"
using namespace std;
using namespace Groups;

const int n = 5;

int main(void) {
 vector<int> cycle1(n);
 for (int i=0; i<n; i++) cycle1[i] = i+1;
 vector<int> cycle2(2);
 cycle2[0] = 2;
 cycle2[1] = 3;
 vector< vector<int> > cycles(2);

 cycles[0] = cycle1;
 cycles[1] = cycle2;
 Permutation p = Permutation::fromCycles(cycles.begin(), cycles.end());
 cout << string(p) << endl;  /* (1 2 4 5) */

 cycles[0] = cycle2;
 cycles[1] = cycle1;
 p = Permutation::fromCycles(cycles.begin(), cycles.end());
 cout << string(p) << endl;  /* (1 3 4 5) */

 return 0;
}
