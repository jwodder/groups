#ifndef BASICGROUP_HPP
#define BASICGROUP_HPP

#include <set>
#include <string>
#include <vector>
#include "closure.hpp"

namespace Groups {
 template<class T> class basic_group {
 public:
  typedef T elem_t;
  virtual ~basic_group() { }
  virtual T oper(const T&, const T&) const = 0;
  virtual T identity() const = 0;
  virtual T invert(const T&) const = 0;
  virtual int order() const = 0;
  virtual int order(const T&) const = 0;
  virtual std::vector<T> elements() const = 0;
  virtual std::string showElem(const T&) const = 0;
  virtual bool isAbelian() const = 0;
  virtual basic_group<T>* copy() const = 0;
  virtual int cmp(const basic_group<T>*) const = 0;
  virtual bool contains(const T&) const = 0;
  virtual int indexElem(const T&) const = 0;

  std::set<T> oper(const T& x, const std::set<T>& ys) const {
   std::set<T> result;
   typename std::set<T>::const_iterator iter;
   for (iter = ys.begin(); iter != ys.end(); iter++) {
    result.insert(oper(x, *iter));
   }
   return result;
  }

  std::set<T> oper(const std::set<T>& xs, const T& y) const {
   std::set<T> result;
   typename std::set<T>::const_iterator iter;
   for (iter = xs.begin(); iter != xs.end(); iter++) {
    result.insert(oper(*iter, y));
   }
   return result;
  }

  std::set<T> oper(const std::set<T>& xs, const std::set<T>& ys) const {
   std::set<T> result;
   typename std::set<T>::const_iterator xiter;
   for (xiter = xs.begin(); xiter != xs.end(); xiter++) {
    typename std::set<T>::const_iterator yiter;
    for (yiter = ys.begin(); yiter != ys.end(); yiter++) {
     result.insert(oper(*xiter, *yiter));
    }
   }
   return result;
  }

  T conjugate(const T& y, const T& x) const {
   return oper(oper(y,x), invert(y));
  }

  std::set<T> conjugate(const T& y, const std::set<T>& xs) const {
   return oper(oper(y,xs), invert(y));
  }

  T pow(const T& x0, int n) const {
   T x = n > 0 ? x0 : invert(x0);
   if (n < 0) n *= -1;
   n %= order(x);
   if (n == 0) return identity();
   int i;
   for (i=1; !(n & i); i <<= 1) x = oper(x,x);
   T agg = x;
   for (i <<= 1, x = oper(x,x); i <= n; i <<= 1, x = oper(x,x))
    if (n & i) agg = oper(agg, x);
   return agg;
  }

  std::set<T> closure(const std::set<T>& start) const {
   return closure2A<T>(opcall(this), start.begin(), start.end());
  }

  template<class Iter>
  std::set<T> closure(Iter first, Iter last) const {
   return closure2A<T>(opcall(this), first, last);
  }

  bool isSubset(const std::set<T>& elems) {
   typename std::set<T>::const_iterator iter;
   for (iter = elems.begin(); iter != elems.end(); iter++) {
    if (!contains(*iter)) return false;
   }
   return true;
  }

 private:
  struct opcall {  // TODO: Look for a better way to accomplish this.
   const basic_group<T>* g;
   opcall(const basic_group<T>* h) : g(h) { }
   T operator()(const T& x, const T& y) const {return g->oper(x,y); }
  };
 };
}

#endif
