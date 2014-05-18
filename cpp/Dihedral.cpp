#include <sstream>
#include <vector>
#include "Dihedral.hpp"
#include "util.hpp"
using namespace std;

namespace Groups {
 Element Dihedral::op(const Element& x, const Element& y) const {
  const delem *xp = getElem<delem>(x), *yp = getElem<delem>(y);
  return mkElem(yp->s ? new delem(!xp->s, (yp->r - xp->r) % n)
   : new delem(xp->s, (xp->r + yp->r) % n));
 }

 Element Dihedral::identity() const {return mkElem(new delem(false, 0)); }

 vector<Element> Dihedral::elements() const {
  vector<Element> elems(2*n, identity());
  vector<Element>::iterator iter = elems.begin();
  int r=1;
  for (iter++; r<n; r++, iter++) *iter = mkElem(new delem(false, r));
  r=0;
  for (; r<n; r++, iter++) *iter = mkElem(new delem(true, r));
  return elems;
 }

 Element Dihedral::invert(const Element& x) const {
  const delem* xp = getElem<delem>(x);
  return xp->s ? x : mkElem(new delem(false, -xp->r % n));
 }

 int Dihedral::order() const {return 2 * n; }

 int Dihedral::order(const Element& x) const {
  const delem* xp = getElem<delem>(x);
  return xp->s ? 2 : n/gcd(xp->r, n);
 }

 string Dihedral::showElem(const Element& x) const {
  const delem* xp = getElem<delem>(x);
  if (!xp->s && xp->r == 0) return "1";
  else {
   ostringstream out;
   if (xp->s) out << "s";
   expgen(out, "r", xp->r, "");
   return out.str();
  }
 }

 bool Dihedral::abelian() const {return n < 3; }

 Dihedral* Dihedral::copy() const {return new Dihedral(n); }
}
