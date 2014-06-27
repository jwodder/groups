#ifndef ELEMENT_H
#define ELEMENT_H

#include <ostream>
#include <stdexcept>  /* invalid_argument */
#include <string>
#include <vector>
#include "Groups/BasicGroup.hpp"

namespace Groups {
 using namespace std;

 class Element;
 template<> class basic_group<Element>;

 class Element {
 public:
  const basic_group<Element>* group() const {return gr; }
  Element         inverse() const;
  int             order()   const;
  Element         pow(int)  const;
  vector<Element> cycle()   const;

  Element  operator*(const Element&) const;
  Element& operator*=(const Element);
  operator string() const;
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

 ostream& operator<<(ostream&, const Element&);
}

#endif
