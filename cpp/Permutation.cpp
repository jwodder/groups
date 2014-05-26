#include <algorithm>  /* max, min, next_permutation, prev_permutation */
#include <functional>  /* greater */
#include <list>
#include <sstream>  /* ostringstream */
#include <stdexcept>  /* logic_error, invalid_argument */
#include <string>
#include <vector>
#include "Permutation.hpp"
#include "util.hpp"  /* lcm, factorial */
using namespace std;

namespace Groups {
 Permutation::Permutation() : pmap(), _even(1), _order(1), _lehmer(0) { }

 Permutation::Permutation(const vector<int>& mapping, int ev, int ord, int lehm)
   : pmap(mapping), _even(ev), _order(ord), _lehmer(lehm) {
  while (!pmap.empty() && pmap.back() == (int) pmap.size()) pmap.pop_back();
 }

 Permutation Permutation::identity() {return Permutation(); }

 int Permutation::operator[](int i) const {
  if (0 < i && i <= (int) pmap.size()) return pmap[i-1];
  else return i;
 }

 Permutation Permutation::operator*(const Permutation& other) const {
  int newdeg = max(degree(), other.degree());
  vector<int> newmap(newdeg);
  for (int i=0; i<newdeg; i++) newmap[i] = (*this)[other[i+1]];
  return Permutation(newmap, _even != -1 && other._even != -1 ? _even == other._even : -1);
 }

 Permutation::operator string() const {
  vector< vector<int> > cycles = toCycles();
  if (cycles.empty()) return string("1");
  else {
   ostringstream out;
   for (size_t i=0; i<cycles.size(); i++) {
    out << '(';
    for (size_t j=0; j<cycles[i].size(); j++) {
     if (j>0) out << ' ';
     out << cycles[i][j];
    }
    out << ')';
   }
   return out.str();
  }
 }

 Permutation::operator bool() const {return !pmap.empty(); }

 int Permutation::degree() const {return pmap.size(); }

 Permutation Permutation::inverse() const {
  vector<int> newmap(pmap.size());
  for (size_t i=0; i<pmap.size(); i++) newmap[pmap[i]-1] = i+1;
  return Permutation(newmap, _even, _order);
 }

 int Permutation::order() const {
  if (_order == -1) {
   _order = 1;
   vector< vector<int> > cycles = toCycles();
   for (size_t i=0; i<cycles.size(); i++) {
    _order = lcm(_order, cycles[i].size());
   }
  }
  return _order;
 }

 bool Permutation::isEven() const {
  if (_even == -1) {
   size_t lensum = 0;
   vector< vector<int> > cycles = toCycles();
   for (size_t i=0; i<cycles.size(); i++) {
    lensum += cycles[i].size() - 1;
   }
   _even = lensum % 2 == 0;
  }
  return bool(_even);
 }

 bool Permutation::isOdd() const {return !isEven(); }

 int Permutation::lehmer() const {
  if (_lehmer == -1) {
   _lehmer = 0;
   list<int> left;
   for (int i=degree(); i>0; i--) left.push_back(i);
   for (int x=degree(); x>0; x--) {
    int y = (*this)[x];
    list<int>::iterator iter = left.begin();
    int i=0;
    while (*iter != y) {iter++; i++; }
    left.erase(iter);
    _lehmer = _lehmer * x + i;
   }
  }
  return _lehmer;
 }

 Permutation Permutation::fromLehmer(int x) {
  int x0 = x;
  vector<int> code;
  for (int f=1; x > 0; f++) {
   code.push_back(x % f);
   x /= f;
  }
  vector<int> mapping;
  mapping.reserve(code.size());
  for (int i=0; i<(int)code.size(); i++) {
   mapping.insert(mapping.begin() + code[i], i+1);
  }
  Permutation p(vector<int>(mapping.rbegin(), mapping.rend()));
  p = p.inverse();
  p._lehmer = max(x0,0);
  return p;
 }

 vector< vector<int> > Permutation::toCycles() const {
  vector<bool> used(degree()+1, false);
  used[0] = true;
  vector< vector<int> > cycles;
  for (;;) {
   int x = -1;
   for (size_t i=0; i<used.size(); i++) {
    if (used[i] == false) {x = i; break; }
   }
   if (x == -1) return cycles;
   vector<int> cyke(1, x);
   used[x] = true;
   int y = (*this)[x];
   while (y != x) {
    cyke.push_back(y);
    used[y] = true;
    y = (*this)[y];
   }
   if (cyke.size() > 1) cycles.push_back(cyke);
  }
 }

 /* Returns the permutation representing the transposition of the positive
  * integers `a` and `b` */
 Permutation Permutation::transposition(int a, int b) {
  if (a < 1 || b < 1) throw invalid_argument("Permutation::transposition: arguments must be positive");
  if (a == b) return Permutation();
  else {
   int big = max(a,b), small = min(a,b);
   vector<int> mapping(big);
   for (int i=0; i<big; i++)
    mapping[i] = i+1 == a ? b : i+1 == b ? a : i+1;
   /* For $a<b$, $Lehmer((a b)) = (b-a) (b-1)! + \sum_{i=a}^{b-2} i!$ */
   int lehmer = 0, fac = factorial(small);
   for (int i=small; i<big-1; i++) {
    lehmer += fac;
    fac *= i+1;
   }
   lehmer += fac * (big-small);
   return Permutation(mapping, 0, 2, lehmer);
  }
 }

 Permutation Permutation::firstOfDegree(int n) {
  if (n <= 1) return identity();
  else return transposition(n, n-1);
 }

 /* This comparison method produces the same ordering as the modified Lehmer
  * codes. */
 int Permutation::cmp(const Permutation& other) const {
  if (degree() < other.degree()) return -1;
  if (degree() > other.degree()) return  1;
  for (int i=degree()-1; i>=0; i--) {
   if (pmap[i] > other.pmap[i]) return -1;
   if (pmap[i] < other.pmap[i]) return 1;
  }
  return 0;
 }

 Permutation& Permutation::operator++() {
  if (degree() == 0) {
   *this = transposition(1,2);
  } else if (!next_permutation(pmap.rbegin(), pmap.rend(), greater<int>())) {
   *this = firstOfDegree(degree()+1);
  } else {
   _even = -1;
   _order = -1;
   if (_lehmer != -1) _lehmer++;
  }
  return *this;
 }

 Permutation Permutation::operator++(int) {
  Permutation tmp(*this);
  ++*this;
  return tmp;
 }

 Permutation& Permutation::operator--() {
  if (degree() == 0)
   throw logic_error("Permutation::operator--: cannot decrement identity");
  prev_permutation(pmap.rbegin(), pmap.rend(), greater<int>());
  while (!pmap.empty() && pmap.back() == (int) pmap.size()) pmap.pop_back();
  _even = -1;
  _order = -1;
  if (_lehmer != -1) _lehmer--;
  return *this;
 }

 Permutation Permutation::operator--(int) {
  Permutation tmp(*this);
  --*this;
  return tmp;
 }
}
