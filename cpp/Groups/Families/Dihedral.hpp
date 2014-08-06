#ifndef DIHEDRAL_H
#define DIHEDRAL_H

#include <stdexcept>
#include <utility>  /* pair */
#include "Groups/BasicGroup.hpp"
#include "Groups/internals.hpp"

namespace Groups {
 class Dihedral : public basic_group< std::pair<bool,int> >,
		  public cmp_with<Dihedral> {
 // The pair is (s,r).
 public:
  Dihedral(int m) : n(m) {
   if (n < 1)
    throw std::invalid_argument("Dihedral(): argument must be positive");
  }

  virtual ~Dihedral() { }
  virtual elem_t oper(const elem_t&, const elem_t&) const;
  virtual elem_t identity() const;
  virtual std::vector<elem_t> elements() const;
  virtual elem_t invert(const elem_t&) const;
  virtual int order() const;
  virtual int order(const elem_t&) const;
  virtual std::string showElem(const elem_t&) const;
  virtual bool isAbelian() const;
  virtual Dihedral* copy() const;
  virtual int cmp(const basic_group<elem_t>*) const;
  virtual bool contains(const elem_t&) const;
  virtual int indexElem(const elem_t&) const;
  virtual int cmp(const Dihedral&) const;
  int getN() const {return n; }
 private:
  int n;
 };
}

#endif
