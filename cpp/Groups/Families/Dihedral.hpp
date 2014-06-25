#ifndef DIHEDRAL_H
#define DIHEDRAL_H

#include <cstdlib>  /* abs */
#include <utility>  /* pair */
#include "Groups/Group.hpp"

namespace Groups {
 class Dihedral : public group< std::pair<bool,int> > {
 // The pair is (s,r).
 public:
  Dihedral(int m) : n(std::abs(m)) { }
  virtual ~Dihedral() { }
  virtual elem_t operator()(const elem_t&, const elem_t&) const;
  virtual elem_t identity() const;
  virtual std::vector<elem_t> elements() const;
  virtual elem_t invert(const elem_t&) const;
  virtual int order() const;
  virtual int order(const elem_t&) const;
  virtual std::string showElem(const elem_t&) const;
  virtual bool abelian() const;
  virtual Dihedral* copy() const;
  virtual int cmp(const group<elem_t>*) const;
  virtual bool contains(const elem_t&) const;
 private:
  int n;
 };
}

#endif
