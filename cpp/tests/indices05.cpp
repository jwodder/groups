#include <iostream>
#include <vector>
#include "Groups/Families/Symmetric.hpp"
using namespace std;
using namespace Groups;

int main() {
 Symmetric g(5);
 typedef Permutation elem_t;
 vector<elem_t> elems = g.elements();
 for (int i=0; i < (int) elems.size(); i++) {
  int j = g.indexElem(elems[i]);
  if (j != i) {
   cout << "Mismatch: " << i << " -> " << g.showElem(elems[i]) << " -> " << j
	<< endl;
  }
 }
 return 0;
}
