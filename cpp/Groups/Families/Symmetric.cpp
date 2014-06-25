#include <vector>
#include "Groups/Families/Symmetric.hpp"
#include "Groups/util.hpp"
using namespace std;

namespace Groups {
 Permutation Symmetric::operator()(const Permutation& x, const Permutation& y)
  const {return x * y; }

 Permutation Symmetric::identity() const {
  return Permutation::identity();
 }

 vector<Permutation> Symmetric::elements() const {
  return Permutation::s_n(degree);
 }

 Permutation Symmetric::invert(const Permutation& x) const {
  return x.inverse();
 }

 int Symmetric::order() const {return factorial(degree); }

 int Symmetric::order(const Permutation& x) const {
  return x.order();
 }

 string Symmetric::showElem(const Permutation& x) const {
  return string(x);
 }

 bool Symmetric::abelian() const {return degree < 3; }

 Symmetric* Symmetric::copy() const {return new Symmetric(degree); }

 int Symmetric::cmp(const group<Permutation>* other) const {
  int ct = cmpTypes(*this, *other);
  if (ct != 0) return ct;
  const Symmetric* c = static_cast<const Symmetric*>(other);
  return degree - c->degree;
 }

 bool Symmetric::contains(const Permutation& x) const {
  return x.degree() <= degree;
 }
}
