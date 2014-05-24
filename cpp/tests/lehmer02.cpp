#include <iostream>
#include "Permutation.hpp"
using namespace std;
using namespace Groups;

int main(void) {
 permutation_gen pg(5);
 permutation_gen::iterator iter;
 for (iter = pg.begin(); iter != pg.end(); iter++) {
  int lehmer = (*iter).lehmer();
  Permutation remhel = Permutation::fromLehmer(lehmer);
  if (*iter != remhel)
   cout << string(*iter) << " -> " << lehmer << " -> " << string(remhel) <<endl;
 }
 return 0;
}
