#ifndef TABULAR_H
#define TABULAR_H

#include "Groups/Group.hpp"

namespace Groups {
 class Tabular : public Group {
 public:
  Tabular(const Group&);
  Tabular(const Tabular&);
  virtual ~Tabular() { }
  virtual Element oper(const Element&, const Element&) const;
  virtual Element identity() const;
  virtual std::vector<Element> elements() const;
  virtual Element invert(const Element&) const;
  virtual int order() const;
  virtual int order(const Element&) const;
  virtual std::string showElem(const Element&) const;
  virtual bool abelian() const;
  virtual Tabular* copy() const;
  virtual int cmp(const Group*) const;
 private:
  std::vector< std::vector<int> > table;
  std::vector<int> inverses, orders;
  std::vector<std::string> strs;
  bool abel;
  typedef int elem_t;
  typedef gelem<elem_t> element;
 };
}

#endif
