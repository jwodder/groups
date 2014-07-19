#ifndef ELEMENT_H
#define ELEMENT_H

#include <ostream>
#include <string>
#include <vector>
#include "Groups/BasicGroup.hpp"
#include "Groups/internals.hpp"

namespace Groups {
 class Element;
 template<> class basic_group<Element>;

 class Element : public cmp_with<Element> {
 public:
  int order() const;
  Element pow(int) const;
  std::vector<Element> cycle() const;

  const basic_group<Element>* group() const {return gr; }
  int index() const {return val; }

  Element  operator*(const Element&) const;
  Element& operator*=(const Element);
  Element  operator~() const;

  operator std::string() const;
  operator bool() const;

  virtual int cmp(const Element& y) const;

 private:
  Element(const basic_group<Element>* g, int x) : gr(g), val(x) { }
  const basic_group<Element>* gr;
  int val;
  friend class basic_group<Element>;
 };

 std::ostream& operator<<(std::ostream&, const Element&);
}

#endif
