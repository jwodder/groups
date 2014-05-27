#ifndef SYMMETRIC_H
#define SYMMETRIC_H

#include <algorithm>  /* max */
#include "Group.hpp"
#include "Permutation.hpp"

namespace Groups {
 class Symmetric : public Group {
 public:
  Symmetric(int d) : degree(std::max(d,1)) { }
  virtual ~Symmetric() { }
  virtual Element op(const Element&, const Element&) const;
  virtual Element identity() const;
  virtual std::vector<Element> elements() const;
  virtual Element invert(const Element&) const;
  virtual int order() const;
  virtual int order(const Element&) const;
  virtual std::string showElem(const Element&) const;
  virtual bool abelian() const;
  virtual Symmetric* copy() const;
 private:
  int degree;
  struct selem : public elem {
   const Permutation s;
   selem(const Permutation& t) : s(t) { }
   virtual int cmp(const elem* p) const {
    //if (typeid(*p) != typeid(*this)) return this - p;
    const selem* x = static_cast<const selem*>(p);
    return s.cmp(x->s);
   }
  };
 };
}

#endif
