/* IMPORTANT TODO: This assumes the identity is always the first element of
 * Group.elements().  Make sure this always holds. */

#include <map>
#include <vector>
#include "Groups/Families/Tabular.hpp"
#include "Groups/util.hpp"
using namespace std;

namespace Groups {
 Tabular::Tabular(const Group& g) {
  vector<Element> gelems = g.elements();
  int qty = g.order();
  map<Element, int> elemdex;
  for (int i=0; i<qty; i++) elemdex[gelems[i]] = i;
  table = vector< vector<int> >(qty, vector<int>(qty));
  for (int i=0; i<qty; i++) {
   for (int j=0; j<qty; j++) {
    table[i][j] = elemdex[gelems[i] * gelems[j]];
   }
  }
  inverses = vector<int>(qty);
  orders = vector<int>(qty);
  strs = vector<string>(qty);
  for (int i=0; i<qty; i++) {
   inverses[i] = elemdex[gelems[i].inverse()];
   orders[i] = gelems[i].order();
   strs[i] = string(gelems[i]);
  }
  abel = g.abelian();
 }

 Tabular::Tabular(const Tabular& t)
  : table(t.table), inverses(t.inverses), orders(t.orders), strs(t.strs),
    abel(t.abel) { }

 Element Tabular::op(const Element& x, const Element& y) const {
  return mkElem<elem_t>(table[getElem<elem_t>(x)][getElem<elem_t>(y)]);
 }

 Element Tabular::identity() const {return mkElem<elem_t>(0); }

 vector<Element> Tabular::elements() const {
  vector<Element> elems(table.size(), identity());
  vector<Element>::iterator iter = elems.begin();
  int i=0;
  for (iter++, i++; iter != elems.end(); iter++, i++) {
   *iter = mkElem<elem_t>(i);
  }
  return elems;
 }

 Element Tabular::invert(const Element& x) const {
  return mkElem<elem_t>(inverses[getElem<elem_t>(x)]);
 }

 int Tabular::order() const {return table.size(); }

 int Tabular::order(const Element& x) const {
  return orders[getElem<elem_t>(x)];
 }

 string Tabular::showElem(const Element& x) const {
  return strs[getElem<elem_t>(x)];
 }

 bool Tabular::abelian() const {return abel; }

 Tabular* Tabular::copy() const {return new Tabular(*this); }

 int Tabular::cmp(const Group* other) const {
  int ct = cmpTypes(*this, *other);
  if (ct != 0) return ct;
  const Tabular* c = static_cast<const Tabular*>(other);
  return table < c->table ? -1 : table > c->table ? 1 : 0;
  /* TODO: Should `strs` also take part in comparisons? */
 }
}
