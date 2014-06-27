#ifndef DICYCLIC_H
#define DICYCLIC_H

#include <cstdlib>  /* abs */
#include <utility>  /* pair */
#include "Groups/BasicGroup.hpp"

namespace Groups {
 class Dicyclic : public basic_group< std::pair<int,bool> > {
 // The pair is (i,j).
 public:
  Dicyclic(int m) : n(std::abs(m)) { }
  virtual ~Dicyclic() { }
  virtual elem_t op(const elem_t&, const elem_t&) const;
  virtual elem_t identity() const;
  virtual std::vector<elem_t> elements() const;
  virtual elem_t invert(const elem_t&) const;
  virtual int order() const;
  virtual int order(const elem_t&) const;
  virtual std::string showElem(const elem_t&) const;
  virtual bool abelian() const;
  virtual Dicyclic* copy() const;
  virtual int cmp(const basic_group<elem_t>*) const;
  virtual bool contains(const elem_t&) const;
 private:
  int n;
 };
}

#endif
