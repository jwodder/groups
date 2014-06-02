#ifndef CYCLIC_H
#define CYCLIC_H

#include <cstdlib>  /* abs */
#include "Groups/Group.hpp"

namespace Groups {
 class Cyclic : public Group {
 public:
  Cyclic(int m) : n(std::abs(m)) { }
  virtual ~Cyclic() { }
  virtual Element op(const Element&, const Element&) const;
  virtual Element identity() const;
  virtual std::vector<Element> elements() const;
  virtual Element invert(const Element&) const;
  virtual int order() const;
  virtual int order(const Element&) const;
  virtual std::string showElem(const Element&) const;
  virtual bool abelian() const;
  virtual Cyclic* copy() const;
  Element residue(int) const;
 private:
  int n;
  typedef int elem_t;
  typedef gelem<elem_t> element;
 };
}

#endif
