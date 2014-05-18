#ifndef CYCLIC_H
#define CYCLIC_H

#include <cstdlib>
#include "Group.hpp"

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
  struct celem : public elem {
   const int x;
   celem(int y) : x(y) { }
   virtual int cmp(const elem* p) const {
    //if (typeid(*p) != typeid(*this)) return this - p;
    const celem* c = static_cast<const celem*>(p);
    return x - c->x;
   }
  };
 };
}

#endif
