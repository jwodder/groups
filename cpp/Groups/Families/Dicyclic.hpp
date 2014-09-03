#ifndef DICYCLIC_H
#define DICYCLIC_H

#include <stdexcept>
#include <utility>  /* pair */
#include "Groups/BasicGroup.hpp"
#include "Groups/internals.hpp"

namespace Groups {
 class Dicyclic : public basic_group< std::pair<int,bool> >,
		  public cmp_with<Dicyclic> {
 // The pair is (i,j).
 public:
  Dicyclic(int m) : n(m) {
   if (n < 2)
    throw std::invalid_argument("Dicyclic(): argument must be at least 2");
  }

  virtual ~Dicyclic() { }
  virtual elem_t oper(const elem_t&, const elem_t&) const;
  virtual elem_t identity() const;
  virtual std::vector<elem_t> elements() const;
  virtual elem_t invert(const elem_t&) const;
  virtual int order() const;
  virtual int order(const elem_t&) const;
  virtual std::string showElem(const elem_t&) const;
  virtual bool isAbelian() const;
  virtual Dicyclic* copy() const;
  virtual int cmp(const basic_group<elem_t>*) const;
  virtual bool contains(const elem_t&) const;
  virtual int indexElem(const elem_t&) const;
  virtual int cmp(const Dicyclic&) const;
  virtual int exponent() const;
  int getN() const {return n; }
 private:
  int n;
 };

 inline Dicyclic* Quaternion(int n=2) {
  if (n < 2)
   throw std::invalid_argument("Quaternion: argument must be at least 2");
  return new Dicyclic(1 << (n-1));
 }
}

#endif
