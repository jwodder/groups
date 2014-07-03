#ifndef SYMMETRIC_H
#define SYMMETRIC_H

#include <algorithm>  /* max */
#include "Groups/Group.hpp"
#include "Permutation.hpp"

namespace Groups {
 class Symmetric : public Group {
 public:
  Symmetric(int d) : degree(std::max(d,1)) { }
  virtual ~Symmetric() { }
  virtual Element oper(const Element&, const Element&) const;
  virtual Element identity() const;
  virtual std::vector<Element> elements() const;
  virtual Element invert(const Element&) const;
  virtual int order() const;
  virtual int order(const Element&) const;
  virtual std::string showElem(const Element&) const;
  virtual bool abelian() const;
  virtual Symmetric* copy() const;
  virtual int cmp(const Group*) const;
 private:
  int degree;
  typedef Permutation elem_t;
  typedef gelem<elem_t> element;
 };
}

#endif
