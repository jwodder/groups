#ifndef DIRECT_H
#define DIRECT_H

#include <utility>  /* pair */
#include "Groups/Group.hpp"

namespace Groups {
 class Direct : public Group {
 public:
  Direct(const Group* g, const Group* h) : left(g->copy()), right(h->copy()) { }
  Direct(const Direct& d) : left(d.left->copy()), right(d.right->copy()) { }
  Direct& operator=(const Direct&);
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
  virtual int cmp(const Group*) const;

	Group* leftGroup()        {return left; }
  const Group* leftGroup()  const {return left; }
	Group* rightGroup()       {return right; }
  const Group* rightGroup() const {return right; }
  Element pair(const Element&, const Element&) const;

 private:
  Group *left, *right;
  typedef std::pair<Element,Element> elem_t;
  typedef gelem<elem_t> element;
 };
}
#endif
