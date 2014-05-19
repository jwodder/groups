#ifndef GROUP_H
#define GROUP_H

#include <string>
#include <vector>
#include "Element.hpp"

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
  inline bool contains(const Element& x) {return x.gr == this; }
 protected:
  Element mkElem(const elem* x) const {return Element(this, x); }
  template<class T> const T* getElem(const Element& x) const {
   if (x.gr == this) return static_cast<const T*>(x.x);
   else throw /* ??? TODO */ ;
  }
 };
}

#endif
