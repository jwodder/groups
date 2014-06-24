#ifndef CYCLIC_H
#define CYCLIC_H

#include <cstdlib>  /* abs */
#include "Groups/Group.hpp"

namespace Groups {
 class Cyclic : public group<int> {
 public:
  Cyclic(int m) : n(std::abs(m)) { }
  virtual ~Cyclic() { }
  virtual int operator()(const int&, const int&) const;
  virtual int identity() const;
  virtual std::vector<int> elements() const;
  virtual int invert(const int&) const;
  virtual int order() const;
  virtual int order(const int&) const;
  virtual std::string showElem(const int&) const;
  virtual bool abelian() const;
  virtual Cyclic* copy() const;
  virtual int cmp(const group<int>*) const;
  virtual bool contains(const int&) const;
  int residue(int) const;
 private:
  int n;
 };
}

#endif
