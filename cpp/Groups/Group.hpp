/* IMPORTANT TODO: This assumes the identity is always the first element of
 * basic_group<T>::elements().  Make sure this always holds. */

#ifndef GROUP_H
#define GROUP_H

#define GROUP_CHECKS_MEMBERSHIP

#include <map>
#include <vector>
#include "Groups/BasicGroup.hpp"
#include "Groups/Element.hpp"
#include "Groups/internals.hpp"

namespace Groups {
 class Group : public basic_group<Element>, public cmp_with<Group> {
 public:

  template<class T> Group(const basic_group<T>& g) {
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
   abel = g.isAbelian();
  }

  Group(const Group& t)
   : table(t.table), inverses(t.inverses), orders(t.orders), strs(t.strs),
     abel(t.abel) { }

  virtual ~Group() { }
	  int oper(const int&, const int&) const;
  virtual Element oper(const Element&, const Element&) const;
  virtual Element identity() const;
  virtual std::vector<Element> elements() const;
	  int invert(const int&) const;
  virtual Element invert(const Element&) const;
  virtual int order() const;
	  int order(const int&) const;
  virtual int order(const Element&) const;
	  std::string showElem(const int&) const;
  virtual std::string showElem(const Element&) const;
  virtual bool isAbelian() const;
  virtual Group* copy() const;
  virtual int cmp(const basic_group<Element>*) const;
	  bool contains(const int&) const;
  virtual bool contains(const Element&) const;
  virtual int indexElem(const Element&) const;
  virtual int cmp(const Group&) const;
	  Group direct(const Group&);

 private:
  Group(std::vector< std::vector<int> > tbl, std::vector<int> invs,
	std::vector<int> ords, std::vector<std::string> ss, bool abelian)
   : table(tbl), inverses(invs), orders(ords), strs(ss), abel(abelian) { }

  std::vector< std::vector<int> > table;
  std::vector<int> inverses, orders;
  std::vector<std::string> strs;
  bool abel;
 };
}

#endif
