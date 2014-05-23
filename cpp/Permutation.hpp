#ifndef PERMUTATION_H
#define PERMUTATION_H

#include <map>
#include <stdexcept>  /* invalid_argument */
#include <string>
#include <vector>

namespace Groups {
 class Permutation {
 public:
  Permutation();
  int  degree() const;
  int  order()  const;
  bool isEven() const;
  bool isOdd()  const;
  int  lehmer() const;
  Permutation inverse() const;
  std::vector< std::vector<int> > toCycles() const;

  int operator[](int) const;
  Permutation operator*(const Permutation&) const;

  operator std::string() const;
  operator bool() const;

  static Permutation identity();
  static Permutation fromLehmer(int);
  static Permutation transposition(int, int);

  int cmp(const Permutation&) const;
  bool operator==(const Permutation& y) const {return cmp(y) == 0; }
  bool operator<(const Permutation& y)  const {return cmp(y) <  0; }
  bool operator>(const Permutation& y)  const {return cmp(y) >  0; }
  bool operator>=(const Permutation& y) const {return cmp(y) >= 0; }
  bool operator<=(const Permutation& y) const {return cmp(y) <= 0; }
  bool operator!=(const Permutation& y) const {return cmp(y) != 0; }

  template<class Iter>
  static Permutation fromCycle(Iter first, Iter last) {
   if (first == last) return Permutation();
   std::map<int,int> mapping;
   int start = *first, maxVal = 0;
   while (first != last) {
    int v = *first;
    if (v < 1) throw std::invalid_argument("Permutation::fromCycle: values must be positive");
    if (mapping.find(v) != mapping.end()) throw std::invalid_argument("Permutation::fromCycle: value appears more than once in cycle");
    first++;
    mapping[v] = first == last ? start : *first;
    if (v > maxVal) maxVal = v;
   }
   if (mapping.size() < 2) return Permutation();
   std::vector<int> vmap(maxVal);
   for (int i=0; i<maxVal; i++) {
    int j = mapping[i+1];
    vmap[i] = j==0 ? i+1 : j;
   }
   return Permutation(vmap);
  }

  template<class Iter>
  static Permutation fromCycles(Iter first, Iter last) {
   Permutation p();
   while (first != last) p *= fromCycle(*first++);
   return p;
  }

 private:
  Permutation(const std::vector<int>&);
  std::vector<int> pmap;
  mutable int _lehmer, _order, _even;
 };
}

#endif
