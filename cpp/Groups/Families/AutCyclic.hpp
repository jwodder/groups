#ifndef AUTCYCLIC_H
#define AUTCYCLIC_H

#include <vector>
#include "Groups/BasicGroup.hpp"
#include "Groups/internals.hpp"

namespace Groups {
 class AutCyclic : public basic_group<int>, public cmp_with<AutCyclic> {
 public:
  AutCyclic(int);
  virtual ~AutCyclic() { }
  virtual int oper(const int&, const int&) const;
  virtual int identity() const;
  virtual std::vector<int> elements() const;
  virtual int invert(const int&) const;
  virtual int order() const;
  virtual int order(const int&) const;
  virtual std::string showElem(const int&) const;
  virtual bool abelian() const;
  virtual AutCyclic* copy() const;
  virtual int cmp(const basic_group<int>*) const;
  virtual bool contains(const int&) const;
  virtual int indexElem(const int&) const;
  virtual int cmp(const AutCyclic&) const;
  int residue(int) const;
 private:
  int n;
  std::vector<int> elems;
 };
}

#endif
