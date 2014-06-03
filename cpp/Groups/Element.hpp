#ifndef ELEMENT_H
#define ELEMENT_H

#include <ostream>
#include <stdexcept>  /* invalid_argument */
#include <string>
#include <vector>

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

 template<class T>
 struct gelem : public elem {
  const T val;
  gelem(const T& v) : val(v) { }
  virtual int cmp(const elem* p) const {
   const gelem<T>* p2 = dynamic_cast<const gelem<T>*>(p);
   if (p2 != NULL) return val < p2->val ? -1 : val > p2->val ? 1 : 0;
   else throw std::invalid_argument("gelem<T>::cmp: group mismatch");
  }
 };

 class Element {
 public:
  Element(const Element& y) : gr(y.gr), x(y.x) {x->retain(); }
  ~Element() {x->release(); }

  const Group*    group()   const {return gr; }
  Element         inverse() const;
  int             order()   const;
  Element         pow(int)  const;
  vector<Element> cycle()   const;

  Element  operator*(const Element&) const;
  Element& operator*=(const Element);
  Element& operator=(const Element&);
  operator string() const;
  operator bool()   const;

  int cmp(const Element& y) const {
   return gr < y.gr ? -1 : gr > y.gr ? 1 : x->cmp(y.x);
  }

  bool operator==(const Element& y) const {return cmp(y) == 0; }
  bool operator<(const Element& y)  const {return cmp(y) <  0; }
  bool operator>(const Element& y)  const {return cmp(y) >  0; }
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
