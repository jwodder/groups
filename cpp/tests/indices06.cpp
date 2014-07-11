#include <iostream>
#include <vector>
#include "Groups/Families/Cyclic.hpp"
#include "Groups/Families/Dicyclic.hpp"
#include "Groups/Families/Direct.hpp"
#include "Groups/Group.hpp"
using namespace std;
using namespace Groups;

int main() {
 Cyclic g1(5);
 Dicyclic g2(3);
 Group g(Direct<int, Dicyclic::elem_t>(g1, g2));
 typedef Element elem_t;
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
