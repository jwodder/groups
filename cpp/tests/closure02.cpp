#include <iostream>
#include "Groups/Families/Cyclic.hpp"
#include "Groups/Grutils.hpp"
using namespace std;
using namespace Groups;

int main(void) {
 Cyclic group(24);
 Element start[1] = {group.residue(3)};
 const set<Element> subgr = closure(start+0, start+1);
 set<Element>::const_iterator iter;
 for (iter = subgr.begin(); iter != subgr.end(); iter++)
  cout << *iter << endl;
 return 0;
}
