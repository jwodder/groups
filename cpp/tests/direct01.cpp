#include <iostream>
#include "Groups/Families/Cyclic.hpp"
#include "Groups/Families/Dicyclic.hpp"
#include "Groups/Group.hpp"
#include "Groups/Util.hpp"
using namespace std;
using namespace Groups;

int main() {
 Dicyclic raw1(2);
 Cyclic raw2(2);
 Group g1(raw1);
 Group g2(raw2);
 showTable(cout, g1.direct(g2));
 return 0;
}
