#ifndef SEMIDIRECT_HPP
#define SEMIDIRECT_HPP

#include <sstream>
#include <utility>  /* pair */
#include "Groups/BasicGroup.hpp"
#include "Groups/Util.hpp"
#include "Groups/internals.hpp"  /* cartesian */

namespace Groups {
 template<class T, class U, class Func>
 class Semidirect : public basic_group< std::pair<T,U> >,
		    public cmp_with< Semidirect<T,U,Func> > {
 public:
  typedef std::pair<T,U> elem_t;  // Why is this necessary?

  Semidirect(const basic_group<T>& g, const basic_group<U>& h, const Func& f)
   : left(g.copy()), right(h.copy()), phi(f) { }

  Semidirect(const basic_group<T>* g, const basic_group<U>* h, const Func& f)
   : left(g->copy()), right(h->copy()), phi(f) { }

  Semidirect(const Semidirect<T,U>& d)
   : left(d.left->copy()), right(d.right->copy()), phi(d.phi) { }

  Semidirect& operator=(const Semidirect& d) {
   if (this != &d) {
    delete left;
    delete right;
    left = d.left->copy();
    right = d.right->copy();
    phi = d.phi;
   }
   return *this;
  }

  virtual ~Semidirect() {delete left; delete right; }

  virtual elem_t oper(const elem_t& x, const elem_t& y) const {
   return elem_t(left->oper(x.first, phi(x.second)(y.first)),
		 right->oper(x.second, y.second));
  }

  virtual elem_t identity() const {
   return elem_t(left->identity(), right->identity());
  }

  virtual std::vector<elem_t> elements() const {
   return cartesian(left->elements(), right->elements());
  }

  virtual elem_t invert(const elem_t& x) const {
   const U& binv = right->invert(x.second);
   return elem_t(phi(binv)(left->invert(x.first)), binv);
  }

  virtual int order() const {return left->order() * right->order(); }

  virtual int order(const elem_t& x) const {
   // Should the results be cached somehow?
   int i = 1;
   elem_t val = x;
   while (val.first != left->identity() && val.second != right->identity()) {
    val = oper(val, x);
    i++;
   }
   if (val.first == left->identity()) return i * right->order(val.second);
   else return i * left->order(val.first);
  }

  virtual std::string showElem(const elem_t& x) const {
   if (x.first == left->identity() && x.second == right->identity()) {
    return "1";
   } else {
    std::ostringstream out;
    out << '(' << left->showElem(x.first) << ", " << right->showElem(x.second)
	<< ')';
    return out.str();
   }
  }

  virtual bool isAbelian() const {
   return left->isAbelian() && right->isAbelian() && ??? ;
  }

  virtual Semidirect<T,U,Func>* copy() const {
   return new Semidirect(left, right, phi);
  }

  virtual int cmp(const basic_group<elem_t>* other) const {
   int ct = cmpTypes(*this, *other);
   if (ct != 0) return ct;
   const Semidirect<T,U,Func>* c
    = static_cast<const Semidirect<T,U,Func>*>(other);
   return cmp(*c);
  }

  virtual int cmp(const Semidirect<T,U>& other) const {
   int cmpLeft = left->cmp(other.left);
   if (cmpLeft != 0) return cmpLeft;
   int cmpRight = right->cmp(other.right);
   if (cmpRight != 0) return cmpRight;
   return phi < other.phi ? -1 : phi > other.phi ? 1 : 0;
  }

  virtual bool contains(const elem_t& x) const {
   return left->contains(x.first) && right->contains(x.second);
  }

  virtual int indexElem(const elem_t& x) const {
   if (contains(x))
    return left->indexElem(x.first)*right->order() + right->indexElem(x.second);
   else throw group_mismatch("Semidirect::indexElem");
  }

//	basic_group<T>* leftGroup()        {return left; }
  const basic_group<T>* leftGroup()  const {return left; }
//	basic_group<U>* rightGroup()       {return right; }
  const basic_group<U>* rightGroup() const {return right; }

  const Func& getPhi() const {return phi; }

  elem_t pair(const T& x, const U& y) const {
   if (left->contains(x) && right->contains(y)) return elem_t(x,y);
   else throw group_mismatch("Semidirect::pair");
  }

 private:
  basic_group<T>* left;
  basic_group<U>* right;
  Func phi;
 };
}

#endif
