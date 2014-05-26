#include <iostream>
#include "Permutation.hpp"
using namespace std;
using namespace Groups;

int main(void) {
 Permutation end = Permutation::firstOfDegree(6);
 for (Permutation p = Permutation::identity(); p < end; p++) {
  int lehmer = p.lehmer();
  Permutation remhel = Permutation::fromLehmer(lehmer);
  if (p != remhel)
   cout << string(p) << " -> " << lehmer << " -> " << string(remhel) << endl;
 }
 return 0;
}
