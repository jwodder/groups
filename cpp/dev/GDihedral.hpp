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
 template<class G>
 class GDihedral : public basic_group< std::pair<typename G::elem_t, bool> >,
		   public cmp_with< GDihedral<G> > {
 public:
  typedef std::pair<typename G::elem_t, bool> elem_t;
  // Should the "bool" element come before the abelian element, as with
  // Dihedral, or should Dihedral have its elements flipped?

  GDihedral(const G& g) : _base(g) {
   if (!g.isAbelian())
    throw std::invalid_argument("GDihedral(): argument must be abelian");
  }

  virtual ~GDihedral() { }

  virtual elem_t oper(const elem_t& x, const elem_t& y) const {
   const T& y1 = x.second ? _base.invert(y.first) : y.first;
   return elem_t(_base.oper(x.first, y1), x.second ^ y.second);
  }

  virtual elem_t identity() const {return elem_t(_base.identity(), false); }

  virtual std::vector<elem_t> elements() const {
   return cartesian(_base.elements(), vecFT);
  }

  virtual elem_t invert(const T& x) const {
   return x.second ? x : elem_t(_base.invert(x.first), false);
  }

  virtual int order() const {return _base.order() * 2; }

  virtual int order(const elem_t& x) const {
   return x.second ? 2 : _base.order(x.first);
  }

  virtual std::string showElem(const elem_t& x) const {
   // TODO: Rethink this.
   if (x.first == _base.identity() && x.second == false) {
    return "1";
   } else {
    std::ostringstream out;
    out << std::boolalpha;
    out << '(' << _base.showElem(x.first) << ", " << x.second << ')';
    return out.str();
   }
  }

  virtual bool isAbelian() const {
   /* `_base` is of the form $E_{2^n}$ for some $n$ */
   ???
  }

  virtual GDihedral<G>* copy() const {return new GDihedral(*this); }

  virtual int cmp(const basic_group<elem_t>* other) const {
   int ct = cmpTypes(*this, *other);
   if (ct != 0) return ct;
   const GDihedral<G>* c = static_cast<const GDihedral<G>*>(other);
   return cmp(*c);
  }

  virtual int cmp(const GDihedral<G>& other) const {
   return _base.cmp(other._base);
  }

  virtual bool contains(const elem_t& x) const {
   return _base.contains(x.first);
  }

  virtual int indexElem(const elem_t& x) const {
   return _base.indexElem(x.first) * 2 + x.second;
  }

  // TODO: Rethink name:
	G& base()       {return _base; }
  const G& base() const {return _base; }

 private:
  G base;
 };
}

#endif
