#include <iostream>
#include "Permutation.hpp"
using namespace std;
using namespace Groups;

int main(void) {
 permutation_gen pg(5);
 permutation_gen::iterator iter;
 for (iter = pg.begin(); iter != pg.end(); iter++)
  cout << iter->lehmer() << '\t' << string(*iter) << endl;
 cout << "---" << endl;
 permutation_gen::reverse_iterator riter;
 for (riter = pg.rbegin(); riter != pg.rend(); riter++)
  //cout << riter->lehmer() << '\t' << string(*riter) << endl;
  cout << (*riter).lehmer() << '\t' << string(*riter) << endl;
 return 0;
}
