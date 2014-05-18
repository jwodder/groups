#ifndef ELEMENT_H
#define ELEMENT_H

#include <string>
#include <ostream>

namespace Groups {
 using namespace std;
 class Group;

 class elem {
 public:
  elem() : refqty(1) { }
  virtual ~elem() { }
  virtual int cmp(const elem*) const = 0;
  const elem* retain() const {refqty++; return this; }
  void release() const {if (--refqty < 1) delete this; }
 private:
  mutable int refqty;
 };

 class Element {
 public:
  Element(const Element& y) : gr(y.gr), x(y.x) {x->retain(); }
  ~Element() {x->release(); }
  const Group* group() const {return gr; }
  Element inverse() const;
  int order() const;
  Element pow(int) const;
  const Element operator*(const Element&) const;
  Element& operator*=(const Element);
  Element& operator=(const Element&);
  operator string() const;

  int cmp(const Element& y) const {
   return gr < y.gr ? -1 : gr > y.gr ? 1 : x->cmp(y.x);
  }

  bool operator==(const Element& y) const {return cmp(y) == 0; }
  bool operator<(const Element& y) const {return cmp(y) < 0; }
  bool operator>(const Element& y) const {return cmp(y) > 0; }
  bool operator>=(const Element& y) const {return cmp(y) >= 0; }
  bool operator<=(const Element& y) const {return cmp(y) <= 0; }
  bool operator!=(const Element& y) const {return cmp(y) != 0; }

 private:
  Element(const Group* g, const elem* e) : gr(g), x(e) { }
  const Group* gr;
  const elem* x;
  friend class Group;
 };

 ostream& operator<<(ostream&, const Element&);
}

#endif
