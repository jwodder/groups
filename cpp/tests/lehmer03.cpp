#include <iostream>
#include "Permutation.hpp"
using namespace std;

int main(void) {
 for (int i=2; i<13; i++) {
  for (int j=1; j<i; j++) {
   Permutation p = Permutation::transposition(i,j);
   int lehmer = p.lehmer();
   Permutation remhel = Permutation::fromLehmer(lehmer);
   //if (p != remhel)
    cout << string(p) << " -> " << lehmer << " -> " << string(remhel) << endl;
  }
 }
 return 0;
}
