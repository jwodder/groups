#include <cstdlib>  /* abs */
#include <algorithm>  /* binary_search, lower_bound */
#include <sstream>
#include <stdexcept>  /* invalid_argument */
#include <vector>
#include "Groups/Families/AutCyclic.hpp"
#include "Groups/Util.hpp"
#include "Groups/internals.hpp"
using namespace std;

namespace Groups {
 AutCyclic::AutCyclic(int m) : n(abs(m)) {
  if (n == 0) throw invalid_argument("AutCyclic(): argument cannot be 0");
  for (int i=1; i<=n; i++) {
   if (gcd(n,i) == 1) elems.push_back(i);
  }
 }

 int AutCyclic::oper(const int& x, const int& y) const {return (x * y) % n; }

 int AutCyclic::identity() const {return 1; }

 vector<int> AutCyclic::elements() const {return elems; }

 int AutCyclic::invert(const int& x) const {
  int i = n, h = residue(x), v = 0, d = 1;
  while (h>0) {
   int t = i/h, x=h;
   h = i % x;
   i = x;
   x = d;
   d = v - t*x;
   v = x;
  }
  return residue(v);
 }

 int AutCyclic::order() const {return elems.size(); }

 int AutCyclic::order(const int& x) const {
  // TODO: Try to find a more efficient way to calculate this.
  int i=1, val = x;
  while (val != 1) {
   val = (val * x) % n;
   i++;
  }
  return i;
 }

 string AutCyclic::showElem(const int& x) const {
  // TODO: Rethink this
  if (x == 1) return "1";
  else {
   ostringstream out;
   out << '*' << x;
   return out.str();
  }
 }

 bool AutCyclic::abelian() const {return true; }

 AutCyclic* AutCyclic::copy() const {return new AutCyclic(n); }

 int AutCyclic::cmp(const basic_group<int>* other) const {
  int ct = cmpTypes(*this, *other);
  if (ct != 0) return ct;
  const AutCyclic* c = static_cast<const AutCyclic*>(other);
  return n - c->n;
 }

 int AutCyclic::residue(int x) const {
  x %= n;
  x = x < 0 ? x+n : x;
  if (contains(x)) return x;
  else throw group_mismatch("AutCyclic::residue");
 }

 bool AutCyclic::contains(const int& x) const {
  return binary_search(elems.begin(), elems.end(), x);
 }

 int AutCyclic::indexElem(const int& x) const {
  vector<int>::const_iterator iter = lower_bound(elems.begin(), elems.end(), x);
  if (iter == elems.end() || *iter != x)
   throw group_mismatch("AutCyclic::indexElem");
  else return iter - elems.begin();
 }
}
