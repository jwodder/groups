#include <sstream>
#include <vector>
#include "Groups/Families/Dihedral.hpp"
#include "Groups/util.hpp"
using namespace std;

namespace Groups {
 typedef Dihedral::elem_t elem_t;

 elem_t Dihedral::oper(const elem_t& x, const elem_t& y) const {
  return y.first ? elem_t(!x.first, (y.second - x.second) % n)
		 : elem_t(x.first,  (x.second + y.second) % n);
 }

 elem_t Dihedral::identity() const {return elem_t(false, 0); }

 vector<elem_t> Dihedral::elements() const {
  vector<elem_t> elems(2*n, identity());
  vector<elem_t>::iterator iter = elems.begin();
  iter++;
  for (int r=1; r<n; r++, iter++) *iter = elem_t(false, r);
  for (int r=0; r<n; r++, iter++) *iter = elem_t(true, r);
  return elems;
 }

 elem_t Dihedral::invert(const elem_t& x) const {
  return x.first ? x : elem_t(false, -x.second % n);
 }

 int Dihedral::order() const {return 2 * n; }

 int Dihedral::order(const elem_t& x) const {
  return x.first ? 2 : n/gcd(x.second, n);
 }

 string Dihedral::showElem(const elem_t& x) const {
  if (!x.first && x.second == 0) return "1";
  else {
   ostringstream out;
   if (x.first) out << "s";
   expgen(out, "r", x.second, "");
   return out.str();
  }
 }

 bool Dihedral::abelian() const {return n < 3; }

 Dihedral* Dihedral::copy() const {return new Dihedral(n); }

 int Dihedral::cmp(const basic_group<elem_t>* other) const {
  int ct = cmpTypes(*this, *other);
  if (ct != 0) return ct;
  const Dihedral* c = static_cast<const Dihedral*>(other);
  return n - c->n;
 }

 bool Dihedral::contains(const elem_t& x) const {
  return 0 <= x.second && x.second < n;
 }
}
