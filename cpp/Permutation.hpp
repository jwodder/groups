#ifndef PERMUTATION_H
#define PERMUTATION_H

#include <cstddef>  /* size_t */
#include <functional>  /* greater */
#include <iterator>
#include <map>
#include <stdexcept>  /* invalid_argument */
#include <string>
#include <vector>

namespace Groups {
 class Permutation;
 /* Declaring permutation_gen before Permutation seems to be necessary for my
  * compiler to accept the `friend` declaration later on. */

 class permutation_gen {
 public:
  class iterator;
  //typedef std::reverse_iterator<iterator> reverse_iterator;

  int degree;
  permutation_gen(int);
  iterator begin() const;
  iterator end() const;
/*
  reverse_iterator rbegin() const;
  reverse_iterator rend() const;
*/

  class iterator
   : std::iterator<std::random_access_iterator_tag, Permutation, size_t, Permutation*, Permutation> {
  public:
   static int iteration_cutoff;
   /* Adding or subtracting more than `iteration_cutoff` from an iterator will
    * be evaluated by converting the resulting Lehmer code to a Permutation.
    * Values less than the cutoff will just use repeated ++ or --. */

   iterator(int=1, bool=false);
   iterator(const permutation_gen&);

   Permutation operator*() const;
   // TODO: Implement `Permutation* operator->() const`?
   iterator& operator++();
   iterator& operator--();

   Permutation operator[](int i) const;

   iterator operator++(int);
   iterator operator--(int);
   iterator& operator+=(int);
   iterator& operator-=(int);
   iterator operator+(int) const;
   iterator operator-(int) const;
   int operator-(const iterator&) const;

   int cmp(const iterator&) const;
   bool operator==(const iterator& y) const {return cmp(y) == 0; }
   bool operator<(const iterator& y)  const {return cmp(y) <  0; }
   bool operator>(const iterator& y)  const {return cmp(y) >  0; }
   bool operator>=(const iterator& y) const {return cmp(y) >= 0; }
   bool operator<=(const iterator& y) const {return cmp(y) <= 0; }
   bool operator!=(const iterator& y) const {return cmp(y) != 0; }

  private:
   const static std::greater<int> comp;
   int degree, lehmer, endLehmer;
   std::vector<int> rmap;
  };
 };

 inline permutation_gen::iterator operator+(int i, const permutation_gen::iterator& x) {return x+i; }

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
  Permutation(const std::vector<int>&, int=-1, int=-1, int=-1);
  std::vector<int> pmap;
  mutable int _even, _order, _lehmer;

  friend class permutation_gen::iterator;
 };
}

#endif
