#include <sstream>
#include <stdexcept>  /* invalid_argument */
#include <vector>
#include "Groups/Families/Direct.hpp"
#include "Groups/util.hpp"
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

 Element Direct::oper(const Element& x, const Element& y) const {
  elem_t xp = getElem<elem_t>(x);
  elem_t yp = getElem<elem_t>(y);
  return mkElem<elem_t>(elem_t(left->oper(xp.first, yp.first),
			       right->oper(xp.second, yp.second)));
 }

 Element Direct::identity() const {
  return mkElem<elem_t>(elem_t(left->identity(), right->identity()));
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
    *iter = mkElem<elem_t>(elem_t(*liter, *riter));
    iter++;
   }
  }
  return elems;
 }

 Element Direct::invert(const Element& x) const {
  elem_t xp = getElem<elem_t>(x);
  return mkElem<elem_t>(elem_t(xp.first.inverse(), xp.second.inverse()));
 }

 int Direct::order() const {return left->order() * right->order(); }

 int Direct::order(const Element& x) const {
  elem_t xp = getElem<elem_t>(x);
  return lcm(xp.first.order(), xp.second.order());
 }

 string Direct::showElem(const Element& x) const {
  elem_t xp = getElem<elem_t>(x);
  if (xp.first == left->identity() && xp.second == right->identity()) {
   return "1";
  } else {
   ostringstream out;
   out << '(' << xp.first << ", " << xp.second << ')';
   return out.str();
  }
 }

 Element Direct::pair(const Element& x, const Element& y) const {
  if (left->contains(x) && right->contains(y))
   return mkElem<elem_t>(elem_t(x,y));
  else throw invalid_argument("Direct::pair: group mismatch");
 }

 bool Direct::abelian() const {return left->abelian() && right->abelian(); }

 Direct* Direct::copy() const {return new Direct(left, right); }

 int Direct::cmp(const Group* other) const {
  int ct = cmpTypes(*this, *other);
  if (ct != 0) return ct;
  const Direct* c = static_cast<const Direct*>(other);
  int cmpLeft = left->cmp(c->left);
  if (cmpLeft != 0) return cmpLeft;
  return right->cmp(c->right);
 }
}
