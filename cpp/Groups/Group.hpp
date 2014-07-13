/* IMPORTANT TODO: This assumes the identity is always the first element of
 * basic_group<T>::elements().  Make sure this always holds. */

#ifndef GROUP_H
#define GROUP_H

#include <map>
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
  virtual bool abelian() const;
  virtual basic_group<Element>* copy() const;
  virtual int cmp(const basic_group<Element>*) const;
	  bool contains(const int&) const;
  virtual bool contains(const Element&) const;
  virtual int indexElem(const Element&) const;
  basic_group<Element> direct(const basic_group<Element>&);

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
