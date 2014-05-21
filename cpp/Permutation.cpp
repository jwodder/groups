#include <algorithm>  /* max */
#include <list>
#include <map>
#include <sstream>
#include <stdexcept>  /* invalid_argument */
#include <string>
#include <vector>
#include "Permutation.hpp"
#include "util.hpp"  /* lcm */
using namespace std;

namespace Groups {
 Permutation::Permutation(const vector<int>& mapping)
   : pmap(mapping), _lehmer(-1), _order(-1), _even(-1) {
  while (!pmap.empty() && pmap.back() == pmap.size()) pmap.pop_back();
 }

 Permutation Permutation::identity() {return Permutation(); }

 int Permutation::operator[](int i) const {
  if (0 < i && i <= pmap.size()) return pmap[i-1];
  else return i;
 }

 Permutation Permutation::operator*(const Permutation& other) const {
  int newdeg = max(degree(), other.degree());
  vector<int> newmap(newdeg);
  for (size_t i=0; i<newdeg; i++) newmap[i] = (*this)[other[i+1]];
  return Permutation(newmap);
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
  return Permutation(newmap);
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
   return Permutation(mapping);
  }
 }
}
