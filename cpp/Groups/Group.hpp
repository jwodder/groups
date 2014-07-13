/* IMPORTANT TODO: This assumes the identity is always the first element of
 * basic_group<T>::elements().  Make sure this always holds. */

#ifndef GROUP_H
#define GROUP_H

#define GROUP_CHECKS_MEMBERSHIP

#include <map>
#include <sstream>
#include <vector>
#include "Groups/BasicGroup.hpp"
#include "Groups/Element.hpp"
#include "Groups/util.hpp"

namespace Groups {
 template<> class basic_group<Element> {
 public:
  template<class T> basic_group(const basic_group<T>& g) {
   std::vector<T> gelems = g.elements();
   int qty = g.order();
   std::map<T,int> elemdex;
   for (int i=0; i<qty; i++) elemdex[gelems[i]] = i;
   table = std::vector< std::vector<int> >(qty, std::vector<int>(qty));
   for (int i=0; i<qty; i++) {
    for (int j=0; j<qty; j++) {
     table[i][j] = elemdex[g.oper(gelems[i], gelems[j])];
    }
   }
   inverses = std::vector<int>(qty);
   orders = std::vector<int>(qty);
   strs = std::vector<std::string>(qty);
   for (int i=0; i<qty; i++) {
    inverses[i] = elemdex[g.invert(gelems[i])];
    orders[i] = g.order(gelems[i]);
    strs[i] = g.showElem(gelems[i]);
   }
   abel = g.abelian();
  }

  basic_group(const basic_group<Element>& t)
   : table(t.table), inverses(t.inverses), orders(t.orders), strs(t.strs),
     abel(t.abel) { }

  virtual ~basic_group() { }

  virtual int oper(const int& x, const int& y) const {return table[x][y]; }

  virtual Element oper(const Element& x, const Element& y) const {
#ifdef GROUP_CHECKS_MEMBERSHIP
   if (!contains(x) || !contains(y)) throw group_mismatch("Group::oper");
#endif
   return Element(this, table[x.val][y.val]);
  }

  virtual Element identity() const {return Element(this, 0); }

  virtual std::vector<Element> elements() const {
   std::vector<Element> elems(table.size(), identity());
   std::vector<Element>::iterator iter = elems.begin();
   int i=0;
   for (iter++, i++; iter != elems.end(); iter++, i++) {
    *iter = Element(this, i);
   }
   return elems;
  }

  virtual int invert(const int& x) const {return inverses[x]; }

  virtual Element invert(const Element& x) const {
#ifdef GROUP_CHECKS_MEMBERSHIP
   if (!contains(x)) throw group_mismatch("Group::invert");
#endif
   return Element(this, inverses[x.val]);
  }

  virtual int order() const {return table.size(); }

  virtual int order(const int& x) const {return orders[x]; }

  virtual int order(const Element& x) const {
#ifdef GROUP_CHECKS_MEMBERSHIP
   if (!contains(x)) throw group_mismatch("Group::order");
#endif
   return orders[x.val];
  }

  virtual std::string showElem(const int& x) const {return strs[x]; }

  virtual std::string showElem(const Element& x) const {
#ifdef GROUP_CHECKS_MEMBERSHIP
   if (!contains(x)) throw group_mismatch("Group::showElem");
#endif
   return strs[x.val];
  }

  virtual bool abelian() const {return abel; }

  virtual basic_group<Element>* copy() const {
   return new basic_group<Element>(*this);
  }

  virtual int cmp(const basic_group<Element>* other) const {
   int ct = cmpTypes(*this, *other);
   if (ct != 0) return ct;
   const basic_group<Element>* c = static_cast<const basic_group<Element>*>(other);
   return table < c->table ? -1 : table > c->table ? 1 : 0;
   /* TODO: Should `strs` also take part in comparisons? */
  }

  virtual bool contains(const int& x) const {return 0 <= x && x < order(); }

  virtual bool contains(const Element& x) const {
   return x.gr->cmp(this) == 0 && 0 <= x.val && x.val < order();
  }

  virtual int indexElem(const Element& x) const {return x.index(); }

  basic_group<Element> direct(const basic_group<Element>& right) {
   int leftQty = order(), rightQty = right.order();
   int newQty = leftQty * rightQty;
   std::vector< std::vector<int> > tbl(newQty, std::vector<int>(newQty));
   for (int i=0; i<newQty; i++) {
    int xa = i / rightQty, xb = i % rightQty;
    for (int j=0; j<newQty; j++) {
     int ya = j / rightQty, yb = j % rightQty;
     tbl[i][j] = table[xa][ya] * rightQty + right.table[xb][yb];
    }
   }
   std::vector<int> invs = std::vector<int>(newQty);
   std::vector<int> ords = std::vector<int>(newQty);
   std::vector<std::string> ss = std::vector<std::string>(newQty);
   for (int i=0; i<newQty; i++) {
    int a = i / rightQty, b = i % rightQty;
    invs[i] = inverses[a] * rightQty + right.inverses[b];
    ords[i] = lcm(orders[a], right.orders[b]);
    if (i == 0) {
     ss[i] = "1";
    } else {
     std::ostringstream out;
     out << '(' << strs[a] << ", " << right.strs[b] << ')';
     ss[i] = out.str();
    }
   }
   return basic_group<Element>(tbl, invs, ords, ss, abel && right.abel);
  }

 private:
  basic_group(std::vector< std::vector<int> > tbl, std::vector<int> invs,
	      std::vector<int> ords, std::vector<std::string> ss, bool abelian)
   : table(tbl), inverses(invs), orders(ords), strs(ss), abel(abelian) { }

  std::vector< std::vector<int> > table;
  std::vector<int> inverses, orders;
  std::vector<std::string> strs;
  bool abel;
 };

 typedef basic_group<Element> Group;
}

#endif
