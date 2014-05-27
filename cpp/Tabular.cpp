/* This assumes the identity is always the first element of Group.elements().
 * Make sure this always holds. */

#include <map>
#include <vector>
#include "Tabular.hpp"
#include "util.hpp"
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
  const telem *xp = getElem<telem>(x), *yp = getElem<telem>(y);
  return mkElem(new telem(table[xp->x][yp->x]));
 }

 Element Tabular::identity() const {return mkElem(new telem(0)); }

 vector<Element> Tabular::elements() const {
  vector<Element> elems(table.size(), identity());
  vector<Element>::iterator iter = elems.begin();
  int i=0;
  for (iter++, i++; iter != elems.end(); iter++, i++) {
   *iter = mkElem(new telem(i));
  }
  return elems;
 }

 Element Tabular::invert(const Element& x) const {
  return mkElem(new telem(inverses[getElem<telem>(x)->x]));
 }

 int Tabular::order() const {return table.size(); }

 int Tabular::order(const Element& x) const {
  return orders[getElem<telem>(x)->x];
 }

 string Tabular::showElem(const Element& x) const {
  return strs[getElem<telem>(x)->x];
 }

 bool Tabular::abelian() const {return abel; }

 Tabular* Tabular::copy() const {return new Tabular(*this); }
}
