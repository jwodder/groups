#include <iostream>
#include "Groups/Families/Cyclic.hpp"
#include "Groups/Families/Direct.hpp"
using namespace std;
using namespace Groups;

int main(void) {
 Cyclic g(8), h(4);
 Direct<int,int> gh(g,h);
 typedef Direct<int,int>::elem_t elem_t;
 elem_t start[1] = {gh.pair(2,1)};
 const set<elem_t> subgr = gh.closure(start+0, start+1);
 set<elem_t>::const_iterator iter;
 for (iter = subgr.begin(); iter != subgr.end(); iter++)
  cout << gh.showElem(*iter) << endl;
 return 0;
}
