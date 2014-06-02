#include <sstream>
#include <vector>
#include "Groups/Families/Cyclic.hpp"
#include "Groups/util.hpp"
using namespace std;

namespace Groups {
 Element Cyclic::op(const Element& x, const Element& y) const {
  elem_t xp = getElem<elem_t>(x), yp = getElem<elem_t>(y);
  return mkElem<elem_t>((xp + yp) % n);
 }

 Element Cyclic::identity() const {return mkElem<elem_t>(0); }

 vector<Element> Cyclic::elements() const {
  vector<Element> elems(n, identity());
  vector<Element>::iterator iter = elems.begin();
  int i=0;
  for (iter++, i++; iter != elems.end(); iter++, i++) {
   *iter = mkElem<elem_t>(i);
  }
  return elems;
 }

 Element Cyclic::invert(const Element& x) const {
  return mkElem<elem_t>(-getElem<elem_t>(x) % n);
 }

 int Cyclic::order() const {return n; }

 int Cyclic::order(const Element& x) const {
  return n/gcd(getElem<elem_t>(x), n);
 }

 string Cyclic::showElem(const Element& x) const {
  ostringstream out;
  expgen(out, "x", getElem<elem_t>(x), "1");
  return out.str();
 }

 bool Cyclic::abelian() const {return true; }

 Cyclic* Cyclic::copy() const {return new Cyclic(n); }

 Element Cyclic::residue(int x) const {return mkElem<elem_t>(x % n); }
}
