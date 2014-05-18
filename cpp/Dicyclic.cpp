#include <sstream>
#include <vector>
#include "Dicyclic.hpp"
#include "util.hpp"
using namespace std;

namespace Groups {
 Element Dicyclic::op(const Element& x, const Element& y) const {
  const delem *xp = getElem<delem>(x), *yp = getElem<delem>(y);
  int i = xp->i + (xp->j ? -1 : 1) * yp->i;
  if (xp->j && yp->j) i += n;
  i %= 2*n;
  return mkElem(new delem(i, xp->j ^ yp->j));
 }

 Element Dicyclic::identity() const {return mkElem(new delem(0, false)); }

 vector<Element> Dicyclic::elements() const {
  vector<Element> elems(4*n, identity());
  vector<Element>::iterator iter = elems.begin();
  int i=1;
  for (iter++; i<2*n; i++, iter++) *iter = mkElem(new delem(i, false));
  i=0;
  for (; i<2*n; i++, iter++) *iter = mkElem(new delem(i, true));
  return elems;
 }

 Element Dicyclic::invert(const Element& x) const {
  const delem* xp = getElem<delem>(x);
  if (xp->j) return mkElem(new delem((xp->i + n) % (2*n), true));
  else return mkElem(new delem(-xp->i % (2*n), false));
 }

 int Dicyclic::order() const {return 4 * n; }

 int Dicyclic::order(const Element& x) const {
  const delem* xp = getElem<delem>(x);
  return xp->j ? 4 : 2*n / gcd(xp->i, 2*n);
 }

 string Dicyclic::showElem(const Element& x) const {
  const delem* xp = getElem<delem>(x);
  ostringstream out;
  int i = xp->i;
  if (i >= n) {out << '-'; i -= n; }
  if (i == 0 && !xp->j) out << "1";
  else {expgen(out, "i", i, ""); if (xp->j) out << 'j'; }
  return out.str();
 }

 bool Dicyclic::abelian() const {return false; }

 Dicyclic* Dicyclic::copy() const {return new Dicyclic(n); }
}
