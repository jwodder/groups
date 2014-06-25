#ifndef GROUP_H
#define GROUP_H
#include <string>
#include <vector>
#include "closure.hpp"
namespace Groups {
 template<class T> class group {
 public:
  typedef T elem_t;
  virtual ~group() { }
  virtual T operator()(const T&, const T&) const = 0;
  virtual T identity() const = 0;
  virtual T invert(const T&) const = 0;
  virtual int order() const = 0;
  virtual int order(const T&) const = 0;
  virtual std::vector<T> elements() const = 0;
  virtual std::string showElem(const T&) const = 0;
  virtual bool abelian() const = 0;
  virtual group<T>* copy() const = 0;
  virtual int cmp(const group<T>*) const = 0;
  virtual bool contains(const T&) const = 0;

  std::set<T> closure(const std::set<T>& start) {
   return closure2A<T>(*this, start.begin(), start.end());
  }

  template<class Iter>
  std::set<T> closure(Iter first, Iter last) {
   return closure2A<T>(*this, first, last);
  }
 };
}
#endif
