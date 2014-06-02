#ifndef DIHEDRAL_H
#define DIHEDRAL_H

#include <cstdlib>  /* abs */
#include <utility>  /* pair */
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
  typedef std::pair<bool,int> elem_t;  /* (s,r) */
  typedef gelem<elem_t> element;
 };
}

#endif
