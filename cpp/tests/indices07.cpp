#include <iostream>
#include <vector>
#include "Groups/Families/AutCyclic.hpp"
using namespace std;
using namespace Groups;

int main() {
 AutCyclic g(16);
 typedef int elem_t;
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
