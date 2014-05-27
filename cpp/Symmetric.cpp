#include <vector>
#include "Symmetric.hpp"
#include "util.hpp"
using namespace std;

namespace Groups {
 Element Symmetric::op(const Element& x, const Element& y) const {
  const selem *xp = getElem<selem>(x), *yp = getElem<selem>(y);
  return mkElem(new selem(xp->s * yp->s));
 }

 Element Symmetric::identity() const {
  return mkElem(new selem(Permutation::identity()));
 }

 vector<Element> Symmetric::elements() const {
  vector<Element> elems(factorial(degree), identity());
  vector<Element>::iterator iter = elems.begin();
  Permutation p = Permutation::identity();
  for (iter++, p++; iter != elems.end(); iter++, p++) {
   *iter = mkElem(new selem(p));
  }
  return elems;
 }

 Element Symmetric::invert(const Element& x) const {
  return mkElem(new selem(getElem<selem>(x)->s.inverse()));
 }

 int Symmetric::order() const {return factorial(degree); }

 int Symmetric::order(const Element& x) const {
  return getElem<selem>(x)->s.order();
 }

 string Symmetric::showElem(const Element& x) const {
  return string(getElem<selem>(x)->s);
 }

 bool Symmetric::abelian() const {return degree < 3; }

 Symmetric* Symmetric::copy() const {return new Symmetric(degree); }
}
