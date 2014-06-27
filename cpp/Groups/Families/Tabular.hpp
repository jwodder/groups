#ifndef TABULAR_H
#define TABULAR_H

#include <map>
#include "Groups/BasicGroup.hpp"

namespace Groups {
 class Tabular : public basic_group<int> {
 public:
  template<class T> Tabular(const basic_group<T>& g) {
   std::vector<T> gelems = g.elements();
   int qty = g.order();
   std::map<T,int> elemdex;
   for (int i=0; i<qty; i++) elemdex[gelems[i]] = i;
   table = std::vector< std::vector<int> >(qty, std::vector<int>(qty));
   for (int i=0; i<qty; i++) {
    for (int j=0; j<qty; j++) {
     table[i][j] = elemdex[g(gelems[i], gelems[j])];
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

  Tabular(const Tabular&);
  virtual ~Tabular() { }
  virtual int op(const int&, const int&) const;
  virtual int identity() const;
  virtual std::vector<int> elements() const;
  virtual int invert(const int&) const;
  virtual int order() const;
  virtual int order(const int&) const;
  virtual std::string showElem(const int&) const;
  virtual bool abelian() const;
  virtual Tabular* copy() const;
  virtual int cmp(const basic_group<int>*) const;
  virtual bool contains(const int&) const;
 private:
  std::vector< std::vector<int> > table;
  std::vector<int> inverses, orders;
  std::vector<std::string> strs;
  bool abel;
 };
}

#endif
