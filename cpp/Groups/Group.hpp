#ifndef GROUP_H
#define GROUP_H
#include <string>
#include <vector>
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
  virtual bool contains(const T& x) const = 0;
 };
}
#endif
