#include <ostream>
#include <string>
#include "Groups/Element.hpp"
#include "Groups/Group.hpp"
using namespace std;

namespace Groups {
 Element Element::operator~() const {return gr->invert(*this); }

 int Element::order() const {return gr->order(*this); }

 Element Element::pow(int n) const {return gr->pow(*this, n); }

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

 Element Element::operator*(const Element& y) const {return gr->oper(*this,y); }

 Element& Element::operator*=(const Element y) {return *this = *this * y; }

 Element::operator string() const {return gr->showElem(*this); }

 Element::operator bool() const {return *this != gr->identity(); }
 // TODO: Why not just `return val != 0;`?

 int Element::cmp(const Element& y) const {
  int c = gr->cmp(*y.gr);
  return c ? c : val - y.val;
 }

 ostream& operator<<(ostream& out, const Element& x) {return out << string(x); }
}
