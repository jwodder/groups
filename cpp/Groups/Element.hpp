#ifndef ELEMENT_H
#define ELEMENT_H

#include <ostream>
#include <string>
#include <vector>
#include "Groups/BasicGroup.hpp"

namespace Groups {
 class Element;
 template<> class basic_group<Element>;

 class Element {
 public:
  const basic_group<Element>* group() const {return gr; }
  int index() const {return val; }

  Element inverse() const;
  int     order()   const;
  Element pow(int)  const;

  std::vector<Element> cycle()   const;

  Element  operator*(const Element&) const;
  Element& operator*=(const Element);
  operator std::string() const;
  operator bool()   const;

  int cmp(const Element& y) const;

  bool operator==(const Element& y) const {return cmp(y) == 0; }
  bool operator<(const Element& y)  const {return cmp(y) <  0; }
  bool operator>(const Element& y)  const {return cmp(y) >  0; }
  bool operator>=(const Element& y) const {return cmp(y) >= 0; }
  bool operator<=(const Element& y) const {return cmp(y) <= 0; }
  bool operator!=(const Element& y) const {return cmp(y) != 0; }

 private:
  Element(const basic_group<Element>* g, int x) : gr(g), val(x) { }
  const basic_group<Element>* gr;
  int val;
  friend class basic_group<Element>;
 };

 std::ostream& operator<<(std::ostream&, const Element&);
}

#endif
