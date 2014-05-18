#include <string>
#include <ostream>
#include "Element.hpp"
#include "Group.hpp"

namespace Groups {
 Element Element::inverse() const {return gr->invert(*this); }

 int Element::order() const {return gr->order(*this); }

 const Element Element::operator*(const Element& y) const {
  return gr->op(*this, y);
 }

 Element& Element::operator*=(const Element y) {return *this = *this * y; }

 Element Element::pow(int n) const {
  Element x(n > 0 ? *this : inverse());
  if (n < 0) n *= -1;
  n %= order();
  if (n == 0) return gr->identity();
  int i;
  for (i=1; !(n & i); i <<= 1) x *= x;
  Element agg(x);
  for (i <<= 1, x *= x; i <= n; i <<= 1, x *= x) if (n & i) agg *= x;
  return agg;
 }

 Element& Element::operator=(const Element& y) {
  gr = y.gr;
  const elem* tmp = y.x->retain();
  x->release();
  x = tmp;
  return *this;
 }

 Element::operator string() const {return gr->showElem(*this); }

 ostream& operator<<(ostream& out, const Element& x) {return out << string(x); }
}
