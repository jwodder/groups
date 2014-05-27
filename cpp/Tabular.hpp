#ifndef TABULAR_H
#define TABULAR_H

#include "Group.hpp"

namespace Groups {
 class Tabular : public Group {
 public:
  Tabular(const Group&);
  Tabular(const Tabular&);
  virtual ~Tabular() { }
  virtual Element op(const Element&, const Element&) const;
  virtual Element identity() const;
  virtual std::vector<Element> elements() const;
  virtual Element invert(const Element&) const;
  virtual int order() const;
  virtual int order(const Element&) const;
  virtual std::string showElem(const Element&) const;
  virtual bool abelian() const;
  virtual Tabular* copy() const;
 private:
  std::vector< std::vector<int> > table;
  std::vector<int> inverses, orders;
  std::vector<std::string> strs;
  bool abel;

  struct telem : public elem {
   const int x;
   telem(int y) : x(y) { }
   virtual int cmp(const elem* p) const {
    //if (typeid(*p) != typeid(*this)) return this - p;
    const telem* c = static_cast<const telem*>(p);
    return x - c->x;
   }
  };
 };
}

#endif
