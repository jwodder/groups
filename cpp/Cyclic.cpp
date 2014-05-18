#include <sstream>
#include <vector>
#include "Cyclic.hpp"
#include "util.hpp"
using namespace std;

namespace Groups {
 Element Cyclic::op(const Element& x, const Element& y) const {
  const celem *xp = getElem<celem>(x), *yp = getElem<celem>(y);
  return mkElem(new celem((xp->x + yp->x) % n));
 }

 Element Cyclic::identity() const {return mkElem(new celem(0)); }

 vector<Element> Cyclic::elements() const {
  vector<Element> elems(n, identity());
  vector<Element>::iterator iter = elems.begin();
  int i=0;
  for (iter++, i++; iter != elems.end(); iter++, i++) {
   *iter = mkElem(new celem(i));
  }
  return elems;
 }

 Element Cyclic::invert(const Element& x) const {
  return mkElem(new celem(-getElem<celem>(x)->x % n));
 }

 int Cyclic::order() const {return n; }

 int Cyclic::order(const Element& x) const {
  return n/gcd(getElem<celem>(x)->x, n);
 }

 string Cyclic::showElem(const Element& x) const {
  const celem* xp = getElem<celem>(x);
  ostringstream out;
  expgen(out, "x", xp->x, "1");
  return out.str();
 }

 bool Cyclic::abelian() const {return true; }

 Cyclic* Cyclic::copy() const {return new Cyclic(n); }

 Element Cyclic::residue(int x) const {return mkElem(new celem(x % n)); }
}
