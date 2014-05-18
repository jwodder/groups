#ifndef DIHEDRAL_H
#define DIHEDRAL_H

#include <cstdlib>
#include "Group.hpp"

namespace Groups {
 class Dihedral : public Group {
 public:
  Dihedral(int m) : n(std::abs(m)) { }
  virtual ~Dihedral() { }
  virtual Element op(const Element&, const Element&) const;
  virtual Element identity() const;
  virtual std::vector<Element> elements() const;
  virtual Element invert(const Element&) const;
  virtual int order() const;
  virtual int order(const Element&) const;
  virtual std::string showElem(const Element&) const;
  virtual bool abelian() const;
  virtual Dihedral* copy() const;
 private:
  int n;
  struct delem : public elem {
   const bool s;
   const int r;
   delem(bool z, int l) : s(z), r(l) { }
   virtual int cmp(const elem* p) const {
    //if (typeid(*p) != typeid(*this)) return this - p;
    const delem* d = static_cast<const delem*>(p);
    return s < d->s ? -1 : s > d->s ? 1 : r - d->r;
   }
  };
 };
}

#endif
