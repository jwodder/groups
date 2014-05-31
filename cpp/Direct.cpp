#include <sstream>
#include <stdexcept>  /* invalid_argument */
#include <vector>
#include "Direct.hpp"
#include "util.hpp"
using namespace std;

namespace Groups {
 Direct& Direct::operator=(const Direct& d) {
  if (this != &d) {
   delete left;
   delete right;
   left = d.left->copy();
   right = d.right->copy();
  }
  return *this;
 }

 Element Direct::op(const Element& x, const Element& y) const {
  const delem* xp = getElem<delem>(x);
  const delem* yp = getElem<delem>(y);
  return mkElem(new delem(left->op(xp->a, yp->a), right->op(xp->b, yp->b)));
 }

 Element Direct::identity() const {
  return mkElem(new delem(left->identity(), right->identity()));
 }

 vector<Element> Direct::elements() const {
  vector<Element> elems(order(), identity()), lems = left->elements(),
   rems = right->elements();
  vector<Element>::iterator iter = elems.begin(), liter = lems.begin(),
   riter = rems.begin();
  iter++;
  riter++;
  for (; liter != lems.end(); liter++, riter = rems.begin()) {
   for (; riter != rems.end(); riter++) {
    *iter = mkElem(new delem(*liter, *riter));
    iter++;
   }
  }
  return elems;
 }

 Element Direct::invert(const Element& x) const {
  const delem* xp = getElem<delem>(x);
  return mkElem(new delem(xp->a.inverse(), xp->b.inverse()));
 }

 int Direct::order() const {return left->order() * right->order(); }

 int Direct::order(const Element& x) const {
  const delem* xp = getElem<delem>(x);
  return lcm(xp->a.order(), xp->b.order());
 }

 string Direct::showElem(const Element& x) const {
  const delem* xp = getElem<delem>(x);
  if (xp->a == left->identity() && xp->b == right->identity()) return "1";
  else {
   ostringstream out;
   out << '(' << xp->a << ", " << xp->b << ')';
   return out.str();
  }
 }

 Element Direct::pair(const Element& x, const Element& y) const {
  if (x.group() == left && y.group() == right) return mkElem(new delem(x, y));
  else throw invalid_argument("Direct::pair: group mismatch");
 }

 bool Direct::abelian() const {return left->abelian() && right->abelian(); }

 Direct* Direct::copy() const {return new Direct(left, right); }
}
