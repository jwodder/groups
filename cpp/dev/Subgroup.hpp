#ifndef SUBGROUP_HPP
#define SUBGROUP_HPP

// TODO: Add a `SUBGROUP_CHECKS_MEMBERSHIP` #def?

#include <iterator>  /* distance */
#include <set>
#include "Groups/BasicGroup.hpp"
#include "Groups/Util.hpp"
#include "Groups/internals.hpp"

namespace Groups {
 template<class T>
 class Subgroup : public basic_group<T>, public cmp_with< Subgroup<T> > {
 // TODO: Should this take the class of the supergroup as a template parameter
 // instead?
 public:

  // TODO: Insert checks in the constructors to ensure `elems` is actually a
  // subgroup! (but make sure `generate` bypasses these checks)

  Subgroup(const basic_group<T>& g, const std::set<T>& els)
   : super(g.copy()), elems(els) { }

  Subgroup(const basic_group<T>* g, const std::set<T>& els)
   : super(g->copy()), elems(els) { }

  Subgroup(const Subgroup<T>& g) : super(g.super->copy()), elems(g.elems) { }

  Subgroup& operator=(const Subgroup& g) {
   if (this != &g) {
    delete super;
    super = g.super->copy();
    elems = g.elems;
   }
   return *this;
  }

  virtual ~Subgroup() {delete super; }

  virtual T oper(const T& x, const T& y) const {return super->oper(x,y); }

  virtual T identity() const {return super->identity(); }

  virtual std::vector<T> elements() const {
   return std::vector<T>(elems.begin(), elems.end());
  }

  virtual T invert(const T& x) const {return super->invert(x); }

  virtual int order() const {return elems.size(); }

  virtual int order(const T& x) const {return super->order(x); }

  virtual std::string showElem(const T& x) const {return super->showElem(x); }

  virtual bool isAbelian() const {
   // TODO: Should this be cached?
   if (super->isAbelian()) return true;
   for (const T& x: elems) {
    for (const T& y: elems) {
     if (super->oper(x,y) != super->oper(y,x)) return false;
    }
   }
   return true;
  }

  virtual Subgroup<T>* copy() const {return new Subgroup(*this); }

  virtual int cmp(const basic_group<T>* other) const {
   int ct = cmpTypes(*this, *other);
   if (ct != 0) return ct;
   const Subgroup<T>* c = static_cast<const Subgroup<T>*>(other);
   return cmp(*c);
  }

  virtual int cmp(const Subgroup<T>& other) const {
   int cmpSuper = super->cmp(other.super);
   if (cmpSuper != 0) return cmpSuper;
   return elems < other.elems ? -1 : elems > other.elems ? 1 : 0;
  }

  virtual bool contains(const T& x) const {return elems.count(x) > 0; }

  virtual int indexElem(const T& x) const {
   std::set<T>::const_iterator iter = elems.lower_bound(x);
   if (iter == elems.end() || *iter != x)
    throw group_mismatch("Subgroup::indexElem");
   else return std::distance(elems.begin(), iter);
  }

  const basic_group<T>* supergroup() const {return super; }

  static Subgroup<T> generate(const basic_group<T>* g, const std::set<T>& a) {
   return Subgroup<T>(g, g->closure(a));
  }

  virtual std::set<T> elementSet() const {return elems; }

  operator std::set<T>() const {return elems; }

 private:
  basic_group<T>* super;
  std::set<T> elems;
 };
}

#endif
