#include <sstream>
#include <string>
#include <vector>
#include "Groups/Group.hpp"
#include "Groups/Util.hpp"  /* group_mismatch */
#include "Groups/internals.hpp"  /* lcm */
using namespace std;

namespace Groups {
 int Group::oper(const int& x, const int& y) const {return table[x][y]; }

 Element Group::oper(const Element& x, const Element& y) const {
#ifdef GROUP_CHECKS_MEMBERSHIP
  if (!contains(x) || !contains(y)) throw group_mismatch("Group::oper");
#endif
  return Element(this, table[x.val][y.val]);
 }

 Element Group::identity() const {return Element(this, 0); }

 vector<Element> Group::elements() const {
  vector<Element> elems(table.size(), identity());
  vector<Element>::iterator iter = elems.begin();
  int i=0;
  for (iter++, i++; iter != elems.end(); iter++, i++) {
   *iter = Element(this, i);
  }
  return elems;
 }

 int Group::invert(const int& x) const {return inverses[x]; }

 Element Group::invert(const Element& x) const {
#ifdef GROUP_CHECKS_MEMBERSHIP
  if (!contains(x)) throw group_mismatch("Group::invert");
#endif
  return Element(this, inverses[x.val]);
 }

 int Group::order() const {return table.size(); }

 int Group::order(const int& x) const {return orders[x]; }

 int Group::order(const Element& x) const {
#ifdef GROUP_CHECKS_MEMBERSHIP
  if (!contains(x)) throw group_mismatch("Group::order");
#endif
  return orders[x.val];
 }

 string Group::showElem(const int& x) const {return strs[x]; }

 string Group::showElem(const Element& x) const {
#ifdef GROUP_CHECKS_MEMBERSHIP
  if (!contains(x)) throw group_mismatch("Group::showElem");
#endif
  return strs[x.val];
 }

 bool Group::abelian() const {return abel; }

 Group* Group::copy() const {return new Group(*this); }

 int Group::cmp(const Group* other) const {
  int ct = cmpTypes(*this, *other);
  if (ct != 0) return ct;
  const Group* c = static_cast<const Group*>(other);
  return cmp(*c);
 }

 int Group::cmp(const Group& other) const {
  return table < other.table ? -1 : table > other.table ? 1 : 0;
  /* TODO: Should `strs` also take part in comparisons? */
 }

 bool Group::contains(const int& x) const {return 0 <= x && x < order(); }

 bool Group::contains(const Element& x) const {
  return x.gr->cmp(this) == 0 && 0 <= x.val && x.val < order();
 }

 int Group::indexElem(const Element& x) const {return x.index(); }

 Group Group::direct(const Group& right) {
  int leftQty = order(), rightQty = right.order();
  int newQty = leftQty * rightQty;
  vector< vector<int> > tbl(newQty, vector<int>(newQty));
  for (int i=0; i<newQty; i++) {
   int xa = i / rightQty, xb = i % rightQty;
   for (int j=0; j<newQty; j++) {
    int ya = j / rightQty, yb = j % rightQty;
    tbl[i][j] = table[xa][ya] * rightQty + right.table[xb][yb];
   }
  }
  vector<int> invs = vector<int>(newQty);
  vector<int> ords = vector<int>(newQty);
  vector<string> ss = vector<string>(newQty);
  for (int i=0; i<newQty; i++) {
   int a = i / rightQty, b = i % rightQty;
   invs[i] = inverses[a] * rightQty + right.inverses[b];
   ords[i] = lcm(orders[a], right.orders[b]);
   if (i == 0) {
    ss[i] = "1";
   } else {
    ostringstream out;
    out << '(' << strs[a] << ", " << right.strs[b] << ')';
    ss[i] = out.str();
   }
  }
  return Group(tbl, invs, ords, ss, abel && right.abel);
 }
}
