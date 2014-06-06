#ifndef GROUP_H
#define GROUP_H

#include <stdexcept>  /* invalid_argument */
#include <string>
#include <vector>
#include "Groups/Element.hpp"

namespace Groups {
 class Group {
 public:
  virtual ~Group() { }
  virtual Element op(const Element&, const Element&) const = 0;
  virtual Element identity() const = 0;
  virtual std::vector<Element> elements() const = 0;
  virtual Element invert(const Element&) const = 0;
  virtual int order() const = 0;
  virtual int order(const Element&) const = 0;
  virtual std::string showElem(const Element&) const = 0;
  virtual bool abelian() const = 0;
  virtual Group* copy() const = 0;
  virtual int cmp(const Group*) const = 0;
  inline bool contains(const Element& x) {return x.gr == this; }
 protected:
  template<class T>
  Element mkElem(const T& x) const {return Element(this, new gelem<T>(x)); }

  template<class T> const T getElem(const Element& x) const {
   if (x.gr == this) return static_cast<const gelem<T>*>(x.x)->val;
   else throw std::invalid_argument("Group::getElem: group mismatch");
  }
 };
}

#endif
