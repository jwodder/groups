#include <sstream>
#include <vector>
#include "Groups/Families/Dicyclic.hpp"
#include "Groups/util.hpp"
using namespace std;

namespace Groups {
 Element Dicyclic::oper(const Element& x, const Element& y) const {
  elem_t xp = getElem<elem_t>(x), yp = getElem<elem_t>(y);
  int i = xp.first + (xp.second ? -1 : 1) * yp.first;
  if (xp.second && yp.second) i += n;
  i %= 2*n;
  return mkElem<elem_t>(elem_t(i, xp.second ^ yp.second));
 }

 Element Dicyclic::identity() const {return mkElem<elem_t>(elem_t(0, false)); }

 vector<Element> Dicyclic::elements() const {
  vector<Element> elems(4*n, identity());
  vector<Element>::iterator iter = elems.begin();
  int i=1;
  for (iter++; i<2*n; i++, iter++) *iter = mkElem<elem_t>(elem_t(i, false));
  i=0;
  for (; i<2*n; i++, iter++) *iter = mkElem<elem_t>(elem_t(i, true));
  return elems;
 }

 Element Dicyclic::invert(const Element& x) const {
  elem_t xp = getElem<elem_t>(x);
  return mkElem<elem_t>(elem_t((xp.second ? xp.first + n : -xp.first) % (2*n),
			       xp.second));
 }

 int Dicyclic::order() const {return 4 * n; }

 int Dicyclic::order(const Element& x) const {
  elem_t xp = getElem<elem_t>(x);
  return xp.second ? 4 : 2*n / gcd(xp.first, 2*n);
 }

 string Dicyclic::showElem(const Element& x) const {
  elem_t xp = getElem<elem_t>(x);
  ostringstream out;
  int i = xp.first;
  if (i >= n) {out << '-'; i -= n; }
  if (i == 0 && !xp.second) out << "1";
  else {expgen(out, "i", i, ""); if (xp.second) out << 'j'; }
  return out.str();
 }

 bool Dicyclic::abelian() const {return false; }

 Dicyclic* Dicyclic::copy() const {return new Dicyclic(n); }

 int Dicyclic::cmp(const Group* other) const {
  int ct = cmpTypes(*this, *other);
  if (ct != 0) return ct;
  const Dicyclic* c = static_cast<const Dicyclic*>(other);
  return n - c->n;
 }
}
