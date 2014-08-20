#include <iostream>
#include <vector>
#include "Groups/Families/Cyclic.hpp"
#include "Groups/Families/Dicyclic.hpp"
#include "Groups/Families/Direct.hpp"
using namespace std;
using namespace Groups;

int main() {
 Cyclic g1(5);
 Dicyclic g2(3);
 Direct<Cyclic, Dicyclic> g(g1, g2);
 typedef Direct<Cyclic, Dicyclic>::elem_t elem_t;
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
