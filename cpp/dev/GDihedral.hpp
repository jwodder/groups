#ifndef GDIHEDRAL_HPP
#define GDIHEDRAL_HPP

#include <ios>  /* boolalpha */
#include <sstream>
#include <stdexcept>
#include <utility>
#include "Groups/BasicGroup.hpp"
#include "Groups/Util.hpp"
#include "Groups/internals.hpp"

namespace Groups {
 template<class T>
 class GDihedral : public basic_group< std::pair<T,bool> >,
		   public cmp_with< GDihedral<T> > {
 // TODO: Should this take the class of the base group as a template parameter
 // instead?
 public:
  typedef std::pair<T,bool> elem_t;
  // Should the "bool" element come before the abelian element, as with
  // Dihedral, or should Dihedral have its elements flipped?

  GDihedral(const basic_group<T>& g) : base(g.copy()) {
   if (!g.isAbelian())
    throw std::invalid_argument("GDihedral(): argument must be abelian");
  }

  GDihedral(const basic_group<T>* g) : base(g->copy()) {
   if (!g->isAbelian())
    throw std::invalid_argument("GDihedral(): argument must be abelian");
  }

  GDihedral(const GDihedral<T>& g) : base(g.base->copy()) { }

  GDihedral<T>& operator=(const GDihedral<T>& g) {
   if (this != &g) {
    delete base;
    base = g.base->copy();
   }
   return *this;
  }

  virtual ~GDihedral() {delete base; }

  virtual elem_t oper(const elem_t& x, const elem_t& y) const {
   const T& y1 = x.second ? base->invert(y.first) : y.first;
   return elem_t(base->oper(x.first, y1), x.second ^ y.second);
  }

  virtual elem_t identity() const {return elem_t(base->identity(), false); }

  virtual std::vector<elem_t> elements() const {
   return cartesian(base->elements(), vecFT);
  }

  virtual elem_t invert(const T& x) const {
   return x.second ? x : elem_t(base->invert(x.first), false);
  }

  virtual int order() const {return base->order() * 2; }

  virtual int order(const elem_t& x) const {
   return x.second ? 2 : base->order(x.first);
  }

  virtual std::string showElem(const elem_t& x) const {
   // TODO: Rethink this.
   if (x.first == base->identity() && x.second == false) {
    return "1";
   } else {
    std::ostringstream out;
    out << std::boolalpha;
    out << '(' << base->showElem(x.first) << ", " << x.second << ')';
    return out.str();
   }
  }

  virtual bool isAbelian() const {
   /* `base` is of the form $E_{2^n}$ for some $n$ */
   ???
  }

  virtual GDihedral<T>* copy() const {return new GDihedral(*this); }

  virtual int cmp(const basic_group<elem_t>* other) const {
   int ct = cmpTypes(*this, *other);
   if (ct != 0) return ct;
   const GDihedral<T>* c = static_cast<const GDihedral<T>*>(other);
   return cmp(*c);
  }

  virtual int cmp(const GDihedral<T>& other) const {
   return base->cmp(other.base);
  }

  virtual bool contains(const elem_t& x) const {
   return base->contains(x.first);
  }

  virtual int indexElem(const elem_t& x) const {
   return base->indexElem(x.first) * 2 + x.second;
  }

  const basic_group<T>* baseGroup() const {return base; }
  // TODO: Rethink name.

 private:
  basic_group<T>* base;
 };
}

#endif
