#ifndef PERMUTATION_H
#define PERMUTATION_H

#include <map>
#include <stdexcept>  /* invalid_argument */
#include <string>
#include <vector>

#if defined(__cplusplus) && __cplusplus >= 201103L
#include <iterator>  /* begin, end */
#endif

namespace Groups {
 class Permutation {
 public:
  Permutation();
  int  degree() const;
  int  order()  const;
  bool isEven() const;
  bool isOdd()  const;
  int  sign()   const;
  int  lehmer() const;
  Permutation inverse() const;
  std::vector< std::vector<int> > toCycles() const;
  bool disjoint(const Permutation& other) const;

  int operator()(int) const;
  Permutation operator*(const Permutation&) const;
  Permutation& operator*=(const Permutation&);

  /* Go to the next/previous Permutation, ordered by modified Lehmer codes */
  Permutation& operator++();
  Permutation  operator++(int);
  Permutation& operator--();
  Permutation  operator--(int);

  operator std::string() const;
  operator bool() const;

  static Permutation identity();
  static Permutation fromLehmer(int);
  static Permutation transposition(int, int);
  static Permutation firstOfDegree(int);

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
   return Permutation(vmap, mapping.size() % 2, mapping.size());
  }

  template<class Iter>
  static Permutation fromCycles(Iter first, Iter last) {
   Permutation p;
   while (first != last) {
#if defined(__cplusplus) && __cplusplus >= 201103L
    p *= fromCycle(std::begin(*first), std::end(*first));
#else
    p *= fromCycle(first->begin(), first->end());
#endif
    first++;
   }
   return p;
  }

 private:
  Permutation(const std::vector<int>&, int=-1, int=-1, int=-1);
  std::vector<int> pmap;
  mutable int _even, _order, _lehmer;
 };
}

#endif
