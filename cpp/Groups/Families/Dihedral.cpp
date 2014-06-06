#include <sstream>
#include <vector>
#include "Groups/Families/Dihedral.hpp"
#include "Groups/util.hpp"
using namespace std;

namespace Groups {
 Element Dihedral::op(const Element& x, const Element& y) const {
  elem_t xp = getElem<elem_t>(x), yp = getElem<elem_t>(y);
  return mkElem<elem_t>(yp.first
   ? elem_t(!xp.first, (yp.second - xp.second) % n)
   : elem_t(xp.first,  (xp.second + yp.second) % n));
 }

 Element Dihedral::identity() const {return mkElem<elem_t>(elem_t(false, 0)); }

 vector<Element> Dihedral::elements() const {
  vector<Element> elems(2*n, identity());
  vector<Element>::iterator iter = elems.begin();
  int r=1;
  for (iter++; r<n; r++, iter++) *iter = mkElem<elem_t>(elem_t(false, r));
  r=0;
  for (; r<n; r++, iter++) *iter = mkElem<elem_t>(elem_t(true, r));
  return elems;
 }

 Element Dihedral::invert(const Element& x) const {
  elem_t xp = getElem<elem_t>(x);
  return xp.first ? x : mkElem<elem_t>(elem_t(false, -xp.second % n));
 }

 int Dihedral::order() const {return 2 * n; }

 int Dihedral::order(const Element& x) const {
  elem_t xp = getElem<elem_t>(x);
  return xp.first ? 2 : n/gcd(xp.second, n);
 }

 string Dihedral::showElem(const Element& x) const {
  elem_t xp = getElem<elem_t>(x);
  if (!xp.first && xp.second == 0) return "1";
  else {
   ostringstream out;
   if (xp.first) out << "s";
   expgen(out, "r", xp.second, "");
   return out.str();
  }
 }

 bool Dihedral::abelian() const {return n < 3; }

 Dihedral* Dihedral::copy() const {return new Dihedral(n); }

 int Dihedral::cmp(const Group* other) const {
  int ct = cmpTypes(*this, *other);
  if (ct != 0) return ct;
  const Dihedral* c = static_cast<const Dihedral*>(other);
  return n - c->n;
 }
}
