#ifndef DIRECT_H
#define DIRECT_H

#include <sstream>
#include <stdexcept>  /* invalid_argument */
#include <utility>  /* pair */
#include "Groups/BasicGroup.hpp"
#include "Groups/util.hpp"

namespace Groups {
 template<class T, class U>
 class Direct : public basic_group< std::pair<T,U> > {
 public:
  typedef std::pair<T,U> elem_t;  // Why is this necessary?

  Direct(const basic_group<T>& g, const basic_group<U>& h)
   : left(g.copy()), right(h.copy()) { }

  Direct(const basic_group<T>* g, const basic_group<U>* h)
   : left(g->copy()), right(h->copy()) { }

  Direct(const Direct<T,U>& d)
   : left(d.left->copy()), right(d.right->copy()) { }

  Direct& operator=(const Direct& d) {
   if (this != &d) {
    delete left;
    delete right;
    left = d.left->copy();
    right = d.right->copy();
   }
   return *this;
  }

  virtual ~Direct() {delete left; delete right; }

  virtual elem_t oper(const elem_t& x, const elem_t& y) const {
   return elem_t(left->oper(x.first, y.first), right->oper(x.second, y.second));
  }

  virtual elem_t identity() const {
   return elem_t(left->identity(), right->identity());
  }

  virtual std::vector<elem_t> elements() const {
   return cartesian(left->elements(), right->elements());
  }

  virtual elem_t invert(const elem_t& x) const {
   return elem_t(left->invert(x.first), right->invert(x.second));
  }

  virtual int order() const {return left->order() * right->order(); }

  virtual int order(const elem_t& x) const {
   return lcm(left->order(x.first), right->order(x.second));
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

  virtual bool abelian() const {return left->abelian() && right->abelian(); }

  virtual Direct<T,U>* copy() const {return new Direct(left, right); }

  virtual int cmp(const basic_group<elem_t>* other) const {
   int ct = cmpTypes(*this, *other);
   if (ct != 0) return ct;
   const Direct<T,U>* c = static_cast<const Direct<T,U>*>(other);
   int cmpLeft = left->cmp(c->left);
   if (cmpLeft != 0) return cmpLeft;
   return right->cmp(c->right);
  }

  virtual bool contains(const elem_t& x) const {
   return left->contains(x.first) && right->contains(x.second);
  }

  virtual int indexElem(const elem_t& x) const {
   if (contains(x))
    return left->indexElem(x.first)*right->order() + right->indexElem(x.second);
   else throw group_mismatch("Direct::indexElem");
  }

	basic_group<T>* leftGroup()        {return left; }
  const basic_group<T>* leftGroup()  const {return left; }
	basic_group<U>* rightGroup()       {return right; }
  const basic_group<U>* rightGroup() const {return right; }

  elem_t pair(const T& x, const U& y) const {
   if (left->contains(x) && right->contains(y)) return elem_t(x,y);
   else throw group_mismatch("Direct::pair");
  }

 private:
  basic_group<T>* left;
  basic_group<U>* right;
 };
}

#endif
