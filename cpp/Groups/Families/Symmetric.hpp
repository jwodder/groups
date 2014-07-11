#ifndef SYMMETRIC_H
#define SYMMETRIC_H

#include <algorithm>  /* max */
#include "Groups/BasicGroup.hpp"
#include "Permutation.hpp"

namespace Groups {
 class Symmetric : public basic_group<Permutation> {
 public:
  Symmetric(int d) : degree(std::max(d,1)) { }
  virtual ~Symmetric() { }
  virtual Permutation oper(const Permutation&, const Permutation&) const;
  virtual Permutation identity() const;
  virtual std::vector<Permutation> elements() const;
  virtual Permutation invert(const Permutation&) const;
  virtual int order() const;
  virtual int order(const Permutation&) const;
  virtual std::string showElem(const Permutation&) const;
  virtual bool abelian() const;
  virtual Symmetric* copy() const;
  virtual int cmp(const basic_group<Permutation>*) const;
  virtual bool contains(const Permutation&) const;
  virtual int indexElem(const Permutation&) const;
 private:
  int degree;
 };
}

#endif
