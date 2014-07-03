#include <vector>
#include "Groups/Families/Symmetric.hpp"
#include "Groups/util.hpp"
using namespace std;

namespace Groups {
 Element Symmetric::oper(const Element& x, const Element& y) const {
  elem_t xp = getElem<elem_t>(x), yp = getElem<elem_t>(y);
  return mkElem<elem_t>(xp * yp);
 }

 Element Symmetric::identity() const {
  return mkElem<elem_t>(Permutation::identity());
 }

/* // I can't get this implementation to compile.
 vector<Element> Symmetric::elements() const {
  vector<Permutation> sn = Permutation::s_n(degree);
  vector<Element> elems(sn.size(), identity());
  transform(sn.begin(), sn.end(), elems.begin(), mkElem<elem_t>);
  return elems;
 }
*/

 vector<Element> Symmetric::elements() const {
  vector<Element> elems(factorial(degree), identity());
  vector<Element>::iterator iter = elems.begin();
  Permutation p = Permutation::identity();
  for (iter++, p++; iter != elems.end(); iter++, p++) {
   *iter = mkElem<elem_t>(p);
  }
  return elems;
 }

 Element Symmetric::invert(const Element& x) const {
  return mkElem<elem_t>(getElem<elem_t>(x).inverse());
 }

 int Symmetric::order() const {return factorial(degree); }

 int Symmetric::order(const Element& x) const {
  return getElem<elem_t>(x).order();
 }

 string Symmetric::showElem(const Element& x) const {
  return string(getElem<elem_t>(x));
 }

 bool Symmetric::abelian() const {return degree < 3; }

 Symmetric* Symmetric::copy() const {return new Symmetric(degree); }

 int Symmetric::cmp(const Group* other) const {
  int ct = cmpTypes(*this, *other);
  if (ct != 0) return ct;
  const Symmetric* c = static_cast<const Symmetric*>(other);
  return degree - c->degree;
 }
}
