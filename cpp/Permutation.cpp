#include <algorithm>  /* max, next_permutation, prev_permutation */
#include <list>
#include <sstream>  /* ostringstream */
#include <stdexcept>  /* logic_error, invalid_argument */
#include <string>
#include <vector>
#include "Permutation.hpp"
#include "util.hpp"  /* lcm */
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
   vector<int> code;
   code.reserve(degree());
   list<int> left;
   for (int i=degree(); i>0; i--) left.push_back(i);
   for (int x=degree(); x>0; x--) {
    int y = (*this)[x];
    list<int>::iterator iter = left.begin();
    int i=0;
    while (*iter != y) {iter++; i++; }
    left.erase(iter);
    code.push_back(i);
   }
   _lehmer = 0;
   int fac = 1, f = 1;
   vector<int>::reverse_iterator riter;
   for (riter = code.rbegin(); riter != code.rend(); riter++) {
    _lehmer += *riter * fac;
    fac *= f;
    f++;
   }
  }
  return _lehmer;
 }

 Permutation Permutation::fromLehmer(int x) {
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
  p._lehmer = max(x,0);
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
   vector<int> mapping(max(a,b));
   for (int i=0; i<max(a,b); i++)
    mapping[i] = i+1 == a ? b : i+1 == b ? a : i+1;
   return Permutation(mapping, 0, 2);
  }
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


 typedef permutation_gen::iterator PGI;

 permutation_gen::permutation_gen(int d) : degree(max(d,1)) { }

 PGI permutation_gen::begin() const {
  return PGI(degree);
 }

 PGI permutation_gen::end() const {
  return PGI(degree, true);
 }

/*
 permutation_gen::reverse_iterator permutation_gen::rbegin() const {
  return permutation_gen::reverse_iterator(end());
 }

 permutation_gen::reverse_iterator permutation_gen::rend() const {
  return permutation_gen::reverse_iterator(begin());
 }
*/

 int PGI::iteration_cutoff = 100;  // I have no idea what this value should be.

 PGI::iterator(int d, bool atend)
   : degree(max(d,1)), lehmer(0), endLehmer(1), rmap(max(d,1)) {
  for (int i=2; i<=degree; i++) endLehmer *= i;
  if (!atend) {
   for (int i=0; i<degree; i++) rmap[i] = degree-i;
  } else {
   lehmer = endLehmer;
   for (int i=0; i<degree; i++) rmap[i] = i+1;
  }
 }

 PGI::iterator(const permutation_gen& pg)
   : degree(pg.degree), lehmer(0), endLehmer(1), rmap(pg.degree) {
  for (int i=2; i<=degree; i++) endLehmer *= i;
  for (int i=0; i<degree; i++) rmap[i] = degree-i;
 }

 PGI& PGI::operator++() {
  if (lehmer < endLehmer && ++lehmer < endLehmer) {
   next_permutation(rmap.begin(), rmap.end(), comp);
  }
  return *this;
 }

 PGI& PGI::operator--() {
  if (lehmer == endLehmer) {lehmer--; }
  else if (lehmer > 0) {
   lehmer--;
   prev_permutation(rmap.begin(), rmap.end(), comp);
  }
  return *this;
 }

 Permutation PGI::operator[](int i) const {return *(*this + i); }

 Permutation PGI::operator*() const {
  if (lehmer == endLehmer)
   throw logic_error("Attempt to dereference exhausted pointer");
  return Permutation(vector<int>(rmap.rbegin(), rmap.rend()), -1, -1, lehmer);
 }

 PGI PGI::operator++(int) {
  iterator tmp = *this;
  ++*this;
  return tmp;
 }

 PGI PGI::operator--(int) {
  iterator tmp = *this;
  --*this;
  return tmp;
 }

 PGI& PGI::operator+=(int i) {
  if (i<0) return *this -= -i;
  if (lehmer+i >= endLehmer) {
   lehmer = endLehmer;
   for (int i=0; i<degree; i++) rmap[i] = i+1;
  } else if (i<iteration_cutoff) {
   for (; i>0; i--) ++*this;
  } else {
   Permutation result = Permutation::fromLehmer(lehmer+i);
   for (int j=0; j<degree; j++) rmap[j] = result[degree-j];
  }
  return *this;
 }

 PGI& PGI::operator-=(int i) {
  if (i<0) return *this += -i;
  if (lehmer-i <= 0) {
   lehmer = 0;
   for (int i=0; i<degree; i++) rmap[i] = degree-i;
  } else if (i<iteration_cutoff) {
   for (; i>0; i--) --*this;
  } else {
   Permutation result = Permutation::fromLehmer(lehmer-i);
   for (int j=0; j<degree; j++) rmap[j] = result[degree-j];
  }
  return *this;
 }

 PGI PGI::operator+(int i) const {
  iterator tmp = *this;
  tmp += i;
  return tmp;
 }

 PGI PGI::operator-(int i) const {
  iterator tmp = *this;
  tmp -= i;
  return tmp;
 }

 int PGI::operator-(const PGI& other) const {
  if (degree != other.degree)
   throw invalid_argument("iterators are from different permutation_gen's");
  return lehmer - other.lehmer;
 }

 int PGI::cmp(const PGI& other) const {
  return degree == other.degree ? lehmer - other.lehmer : other.degree - degree;
 }
}
