/* IMPORTANT TODO: This assumes the identity is always the first element of
 * Group.elements().  Make sure this always holds. */

#include <vector>
#include "Groups/Families/Tabular.hpp"
#include "Groups/util.hpp"
using namespace std;

namespace Groups {
 Tabular::Tabular(const Tabular& t)
  : table(t.table), inverses(t.inverses), orders(t.orders), strs(t.strs),
    abel(t.abel) { }

 int Tabular::operator()(const int& x, const int& y) const {
  return table[x][y];
 }

 int Tabular::identity() const {return 0; }

 vector<int> Tabular::elements() const {
  vector<int> elems(table.size(), 0);
  vector<int>::iterator iter = elems.begin();
  int i=0;
  for (iter++, i++; iter != elems.end(); iter++, i++) {
   *iter = i;
  }
  return elems;
 }

 int Tabular::invert(const int& x) const {return inverses[x]; }

 int Tabular::order() const {return table.size(); }

 int Tabular::order(const int& x) const {return orders[x]; }

 string Tabular::showElem(const int& x) const {return strs[x]; }

 bool Tabular::abelian() const {return abel; }

 Tabular* Tabular::copy() const {return new Tabular(*this); }

 int Tabular::cmp(const group<int>* other) const {
  int ct = cmpTypes(*this, *other);
  if (ct != 0) return ct;
  const Tabular* c = static_cast<const Tabular*>(other);
  return table < c->table ? -1 : table > c->table ? 1 : 0;
  /* TODO: Should `strs` also take part in comparisons? */
 }

 bool Tabular::contains(const int& x) const {
  return 0 <= x && x < order();
 }
}
