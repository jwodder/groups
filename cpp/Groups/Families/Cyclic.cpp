#include <sstream>
#include <vector>
#include "Groups/Families/Cyclic.hpp"
#include "Groups/Util.hpp"
#include "Groups/internals.hpp"
using namespace std;

namespace Groups {
 int Cyclic::oper(const int& x, const int& y) const {return (x + y) % n; }

 int Cyclic::identity() const {return 0; }

 vector<int> Cyclic::elements() const {return vecN(n); }

 int Cyclic::invert(const int& x) const {return -x % n; }

 int Cyclic::order() const {return n; }

 int Cyclic::order(const int& x) const {return n/gcd(x,n); }

 string Cyclic::showElem(const int& x) const {
  ostringstream out;
  expgen(out, "x", x, "1");
  return out.str();
 }

 bool Cyclic::abelian() const {return true; }

 Cyclic* Cyclic::copy() const {return new Cyclic(n); }

 int Cyclic::cmp(const basic_group<int>* other) const {
  int ct = cmpTypes(*this, *other);
  if (ct != 0) return ct;
  const Cyclic* c = static_cast<const Cyclic*>(other);
  return n - c->n;
 }

 int Cyclic::residue(int x) const {
  x %= n;
  return x < 0 ? x+n : x;
 }

 bool Cyclic::contains(const int& x) const {return 0 <= x && x < n; }

 int Cyclic::indexElem(const int& x) const {
  if (contains(x)) return x;
  else throw group_mismatch("Cyclic::indexElem");
 }
}
