#ifndef DICYCLIC_H
#define DICYCLIC_H

#include <cstdlib>
#include "Group.hpp"

namespace Groups {
 class Dicyclic : public Group {
 public:
  Dicyclic(int m) : n(std::abs(m)) { }
  virtual ~Dicyclic() { }
  virtual Element op(const Element&, const Element&) const;
  virtual Element identity() const;
  virtual std::vector<Element> elements() const;
  virtual Element invert(const Element&) const;
  virtual int order() const;
  virtual int order(const Element&) const;
  virtual std::string showElem(const Element&) const;
  virtual bool abelian() const;
  virtual Dicyclic* copy() const;
 private:
  int n;
  struct delem : public elem {
   const int i;
   const bool j;
   delem(int k, bool l) : i(k), j(l) { }
   virtual int cmp(const elem* p) const {
    //if (typeid(*p) != typeid(*this)) return this - p;
    const delem* d = static_cast<const delem*>(p);
    return i == d->i ? j - d->j : i - d->i;
   }
  };
 };
}

#endif
