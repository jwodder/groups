#include <iostream>
#include "Groups/Families/Cyclic.hpp"
#include "Groups/Families/Dicyclic.hpp"
#include "Groups/Families/Direct.hpp"
#include "Groups/Group.hpp"
#include "Groups/Util.hpp"
using namespace std;
using namespace Groups;

int main() {
 Dicyclic raw1(2);
 Cyclic raw2(2);
 Direct<pair<int,bool>, int> raw3(raw1, raw2);
 Group g(raw3);
 showTable(cout, g);
 return 0;
}
