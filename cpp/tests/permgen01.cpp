#include <iostream>
#include "Permutation.hpp"
using namespace std;
using namespace Groups;

int main(void) {
 permutation_gen pg(5);
 permutation_gen::iterator iter;
 for (iter = pg.begin(); iter != pg.end(); iter++)
  //cout << iter->lehmer() << '\t' << string(*iter) << endl;
  cout << (*iter).lehmer() << '\t' << string(*iter) << endl;
 return 0;
}
