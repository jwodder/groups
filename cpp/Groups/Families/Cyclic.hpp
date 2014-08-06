#ifndef CYCLIC_H
#define CYCLIC_H

#include <cstdlib>  /* abs */
#include <stdexcept>
#include "Groups/BasicGroup.hpp"
#include "Groups/internals.hpp"

namespace Groups {
 class Cyclic : public basic_group<int>, public cmp_with<Cyclic> {
 public:
  Cyclic(int m) : n(std::abs(m)) {
   if (n == 0) throw std::invalid_argument("Cyclic(): argument cannot be 0");
  }

  virtual ~Cyclic() { }
  virtual int oper(const int&, const int&) const;
  virtual int identity() const;
  virtual std::vector<int> elements() const;
  virtual int invert(const int&) const;
  virtual int order() const;
  virtual int order(const int&) const;
  virtual std::string showElem(const int&) const;
  virtual bool isAbelian() const;
  virtual Cyclic* copy() const;
  virtual int cmp(const basic_group<int>*) const;
  virtual bool contains(const int&) const;
  virtual int indexElem(const int&) const;
  virtual int cmp(const Cyclic&) const;
  int residue(int) const;
  int getN() const {return n; }
 private:
  int n;
 };
}

#endif
