#include <vector>
#include "Groups/Families/Alternating.hpp"
#include "Groups/Util.hpp"
#include "Groups/internals.hpp"  /* factorial */
using namespace std;

namespace Groups {
 Permutation Alternating::oper(const Permutation& x, const Permutation& y) const {
  return x * y;
 }

 Permutation Alternating::identity() const {return Permutation::identity(); }

 vector<Permutation> Alternating::elements() const {
  vector<Permutation> elems(order(), identity());
  vector<Permutation>::iterator iter = elems.begin();
  Permutation p = *iter;
  for (iter++, p++; iter != elems.end(); iter++, p++) {
   while (p.isOdd()) p++;
   *iter = p;
  }
  return elems;
 }


 Permutation Alternating::invert(const Permutation& x) const {
  return x.inverse();
 }

 int Alternating::order() const {
  return degree == 1 ? 1 : factorial(degree) / 2;
 }

 int Alternating::order(const Permutation& x) const {return x.order(); }

 string Alternating::showElem(const Permutation& x) const {return string(x); }

 bool Alternating::abelian() const {return degree < 4; }

 Alternating* Alternating::copy() const {return new Alternating(degree); }

 int Alternating::cmp(const basic_group<Permutation>* other) const {
  int ct = cmpTypes(*this, *other);
  if (ct != 0) return ct;
  const Alternating* c = static_cast<const Alternating*>(other);
  return cmp(*c);
 }

 int Alternating::cmp(const Alternating& other) const {
  return degree - other.degree;
 }

 bool Alternating::contains(const Permutation& x) const {
  return x.degree() <= degree && x.isEven();
 }

 int Alternating::indexElem(const Permutation& x) const {
  ???
 }
}
