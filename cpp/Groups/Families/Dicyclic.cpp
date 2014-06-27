#include <sstream>
#include <vector>
#include "Groups/Families/Dicyclic.hpp"
#include "Groups/util.hpp"
using namespace std;

namespace Groups {
 typedef Dicyclic::elem_t elem_t;

 elem_t Dicyclic::op(const elem_t& x, const elem_t& y) const {
  int i = x.first + (x.second ? -y.first : y.first);
  if (x.second && y.second) i += n;
  i %= 2*n;
  return elem_t(i, x.second ^ y.second);
 }

 elem_t Dicyclic::identity() const {return elem_t(0, false); }

 vector<elem_t> Dicyclic::elements() const {
  vector<elem_t> elems(4*n, identity());
  vector<elem_t>::iterator iter = elems.begin();
  int i=1;
  for (iter++; i<2*n; i++, iter++) *iter = elem_t(i, false);
  i=0;
  for (; i<2*n; i++, iter++) *iter = elem_t(i, true);
  return elems;
 }

 elem_t Dicyclic::invert(const elem_t& x) const {
  return elem_t((x.second ? x.first + n : -x.first) % (2*n), x.second);
 }

 int Dicyclic::order() const {return 4 * n; }

 int Dicyclic::order(const elem_t& x) const {
  return x.second ? 4 : 2*n / gcd(x.first, 2*n);
 }

 string Dicyclic::showElem(const elem_t& x) const {
  ostringstream out;
  int i = x.first;
  if (i >= n) {out << '-'; i -= n; }
  if (i == 0 && !x.second) out << "1";
  else {
   expgen(out, "i", i, "");
   if (x.second) out << 'j';
  }
  return out.str();
 }

 bool Dicyclic::abelian() const {return false; }

 Dicyclic* Dicyclic::copy() const {return new Dicyclic(n); }

 int Dicyclic::cmp(const basic_group<elem_t>* other) const {
  int ct = cmpTypes(*this, *other);
  if (ct != 0) return ct;
  const Dicyclic* c = static_cast<const Dicyclic*>(other);
  return n - c->n;
 }

 bool Dicyclic::contains(const elem_t& x) const {
  return 0 <= x.first && x.first < 2*n;
 }
}
