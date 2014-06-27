#include <string>
#include <ostream>
#include "Groups/Element.hpp"
#include "Groups/Group.hpp"

namespace Groups {
 Element Element::inverse() const {return gr->invert(*this); }

 int Element::order() const {return gr->order(*this); }

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

 vector<Element> Element::cycle() const {
  int qty = order();
  vector<Element> pows(qty, gr->identity());
  Element tmp(*this);
  for (int i=1; i<qty; i++) {
   pows[i] = tmp;
   tmp *= *this;
  }
  return pows;
 }

 Element Element::operator*(const Element& y) const {return gr->op(*this, y); }

 Element& Element::operator*=(const Element y) {return *this = *this * y; }

 Element::operator string() const {return gr->showElem(*this); }

 Element::operator bool() const {return *this != gr->identity(); }

 int Element::cmp(const Element& y) const {
  int c = gr->cmp(y.gr);
  if (c == 0) return val - y.val;
  else return c;
 }

 ostream& operator<<(ostream& out, const Element& x) {return out << string(x); }
}
