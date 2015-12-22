#ifndef BASICGROUP_HPP
#define BASICGROUP_HPP

#include <set>
#include <string>
#include <vector>
#include "closure.hpp"
#include "Groups/internals.hpp"

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
  virtual basic_group<T>* copy() const = 0;
  virtual int cmp(const basic_group<T>*) const = 0;
  virtual bool contains(const T&) const = 0;
  virtual int indexElem(const T&) const = 0;

  virtual bool isAbelian() const {
   const std::vector<T>& elems = elements();
   typename std::vector<T>::const_iterator xiter;
   for (xiter = elems.begin(); xiter != elems.end(); xiter++) {
    typename std::vector<T>::const_iterator yiter = xiter;
    for (yiter++; yiter != elems.end(); yiter++) {
     if (oper(*xiter, *yiter) != oper(*yiter, *xiter)) return false;
    }
   }
   return true;
  }

  virtual std::set<T> elementSet() const {
   const std::vector<T>& elems = elements();
   return std::set<T>(elems.begin(), elems.end());
  }

  std::set<T> oper(const T& x, const std::set<T>& ys) const {
   return mapset([&x](const T& y) -> T {return oper(x,y); }, ys);
  }

  std::set<T> oper(const std::set<T>& xs, const T& y) const {
   return mapset([&y](const T& x) -> T {return oper(x,y); }, xs);
  }

  std::set<T> oper(const std::set<T>& xs, const std::set<T>& ys) const {
   std::set<T> result;
   for (const T& x: xs) {
    for (const T& y: ys) {
     result.insert(oper(x,y));
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

  bool isSubset(const std::set<T>& elems) const {
   for (const T& x: elems) {
    if (!contains(x)) return false;
   }
   return true;
  }

  bool isSubgroup(const std::set<T>& elems) const {
   if (elems.empty() || !isSubset(elems)) return false;
   for (const T& x: elems) {
    for (const T& y: elems) {
     if (elems.count(oper(x,y)) == 0) return false;
    }
   }
   return true;
  }

  virtual int exponent() const {
   int ex = 1;
   for (const T& x: elements()) {
    ex = lcm(ex, order(x));
   }
   return ex;
  }

  std::set<T> cycle(const T& x) const {
   // TODO: Rename this to `cycleSet` in order to keep its return type from
   // being confused with that of `Element::cycle`?
   const T& id = identity();
   std::set<T> cyke;
   cyke.insert(id);
   T y = x;
   while (y != id) {
    cyke.insert(y);
    y = oper(y,x);
   }
   return cyke;
  }

  template<class Iter>
  std::set<T> centralizer(Iter first, Iter last) const {
   std::set<T> elems = elementSet();
   for (; first != last; first++) {
    typename std::set<T>::iterator iter;
    for (iter = elems.begin(); iter != elems.end(); ) {
     if (oper(*first, *iter) != oper(*iter, *first)) elems.erase(iter++);
     else iter++;
    }
   }
   return elems;
  }

  std::set<T> centralizer(const std::set<T>& xs) const {
   return centralizer(xs.begin(), xs.end());
  }

  std::set<T> center() const {
   const std::vector<T>& elems = elements();
   return centralizer(elems.begin(), elems.end());
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
