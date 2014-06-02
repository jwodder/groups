#ifndef DICYCLIC_H
#define DICYCLIC_H

#include <cstdlib>  /* abs */
#include <utility>  /* pair */
#include "Groups/Group.hpp"

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
  typedef std::pair<int,bool> elem_t;  /* (i,j) */
  typedef gelem<elem_t> element;
 };
}

#endif
