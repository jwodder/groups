#ifndef DIRECT_H
#define DIRECT_H

#include "Group.hpp"

namespace Groups {
 class Direct : public Group {
 public:
  Direct(const Group* g, const Group* h) : left(g->copy()), right(h->copy()) { }
  virtual ~Direct() {delete left; delete right; }
  virtual Element op(const Element&, const Element&) const;
  virtual Element identity() const;
  virtual std::vector<Element> elements() const;
  virtual Element invert(const Element&) const;
  virtual int order() const;
  virtual int order(const Element&) const;
  virtual std::string showElem(const Element&) const;
  virtual bool abelian() const;
  virtual Direct* copy() const;

  Group* leftGroup() {return left; }
  const Group* leftGroup() const {return left; }
  Group* rightGroup() {return right; }
  const Group* rightGroup() const {return right; }
  Element pair(const Element&, const Element&) const;

 private:
  Group *left, *right;
  struct delem : public elem {
   const Element a, b;
   delem(const Element& x, const Element& y) : a(x), b(y) { }
   virtual int cmp(const elem* p) const {
    //if (typeid(*p) != typeid(*this)) return this < p;
    const delem* d = static_cast<const delem*>(p);
    int ca = a.cmp(d->a);
    return ca == 0 ? b.cmp(d->b) : ca;
   }
  };
 };
}
#endif
