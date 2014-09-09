#ifndef SUBGROUP_HPP
#define SUBGROUP_HPP

// TODO: Add a `SUBGROUP_CHECKS_MEMBERSHIP` #def?

#include <iterator>  /* distance */
#include <set>
#include "Groups/BasicGroup.hpp"
#include "Groups/Util.hpp"
#include "Groups/internals.hpp"

namespace Groups {
 template<class G>
 class Subgroup : public basic_group<typename G::elem_t>,
		  public cmp_with< Subgroup<G> > {
 // TODO: Somehow make `Subgroup<Subgroup<G>>` a synonym for `Subgroup<G>`
 public:
  typedef typename G::elem_t elem_t;

  // TODO: Insert checks in the constructors to ensure `elems` is actually a
  // subgroup! (but make sure `generate` bypasses these checks)

  Subgroup(const G& g, const std::set<elem_t>& els) : super(g), elems(els) { }

  virtual ~Subgroup() { }

  virtual elem_t oper(const elem_t& x, const elem_t& y) const {
   return super->oper(x,y);
  }

  virtual elem_t identity() const {return super->identity(); }

  virtual std::vector<elem_t> elements() const {
   return std::vector<elem_t>(elems.begin(), elems.end());
  }

  virtual elem_t invert(const elem_t& x) const {return super->invert(x); }

  virtual int order() const {return elems.size(); }

  virtual int order(const elem_t& x) const {return super->order(x); }

  virtual std::string showElem(const elem_t& x) const {
   return super->showElem(x);
  }

  virtual bool isAbelian() const {
   return super->isAbelian() || basic_group<elem_t>::isAbelian();
  }

  virtual Subgroup<G>* copy() const {return new Subgroup(*this); }

  virtual int cmp(const basic_group<elem_t>* other) const {
   int ct = cmpTypes(*this, *other);
   if (ct != 0) return ct;
   const Subgroup<G>* c = static_cast<const Subgroup<G>*>(other);
   return cmp(*c);
  }

  virtual int cmp(const Subgroup<G>& other) const {
   int cmpSuper = super->cmp(other.super);
   if (cmpSuper != 0) return cmpSuper;
   return elems < other.elems ? -1 : elems > other.elems ? 1 : 0;
  }

  virtual bool contains(const elem_t& x) const {return elems.count(x) > 0; }

  virtual int indexElem(const elem_t& x) const {
   std::set<elem_t>::const_iterator iter = elems.lower_bound(x);
   if (iter == elems.end() || *iter != x)
    throw group_mismatch("Subgroup::indexElem");
   else return std::distance(elems.begin(), iter);
  }

  const G& supergroup() const {return super; }

  static Subgroup<G> generate(const G& g, const std::set<elem_t>& a) {
   return Subgroup<G>(g, g->closure(a));
  }

  virtual std::set<elem_t> elementSet() const {return elems; }

  operator std::set<elem_t>() const {return elems; }

 private:
  G super;
  std::set<elem_t> elems;
 };
}

#endif
