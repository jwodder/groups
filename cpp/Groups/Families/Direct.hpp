#ifndef DIRECT_H
#define DIRECT_H

#include <sstream>
#include <utility>  /* pair */
#include "Groups/BasicGroup.hpp"
#include "Groups/Util.hpp"
#include "Groups/internals.hpp"  /* cartesian, lcm */

namespace Groups {
 template<class G, class H>
 class Direct : public basic_group< std::pair<typename G::elem_t,
					      typename H::elem_t> >,
		public cmp_with< Direct<G,H> > {
 public:
  typedef typename G::elem_t left_t;
  typedef typename H::elem_t right_t;
  typedef std::pair<left_t, right_t> elem_t;  // Why is this necessary?

  Direct(const G& g, const H& h) : _left(g), _right(h) { }

  virtual ~Direct() { }

  virtual elem_t oper(const elem_t& x, const elem_t& y) const {
   return elem_t(_left.oper(x.first, y.first), _right.oper(x.second, y.second));
  }

  virtual elem_t identity() const {
   return elem_t(_left.identity(), _right.identity());
  }

  virtual std::vector<elem_t> elements() const {
   return cartesian(_left.elements(), _right.elements());
  }

  virtual elem_t invert(const elem_t& x) const {
   return elem_t(_left.invert(x.first), _right.invert(x.second));
  }

  virtual int order() const {return _left.order() * _right.order(); }

  virtual int order(const elem_t& x) const {
   return lcm(_left.order(x.first), _right.order(x.second));
  }

  virtual std::string showElem(const elem_t& x) const {
   if (x.first == _left.identity() && x.second == _right.identity()) {
    return "1";
   } else {
    std::ostringstream out;
    out << '(' << _left.showElem(x.first) << ", " << _right.showElem(x.second)
	<< ')';
    return out.str();
   }
  }

  virtual bool isAbelian() const {
   return _left.isAbelian() && _right.isAbelian();
  }

  virtual Direct<G,H>* copy() const {return new Direct<G,H>(_left, _right); }

  virtual int cmp(const basic_group<elem_t>* other) const {
   int ct = cmpTypes(*this, *other);
   if (ct != 0) return ct;
   const Direct<G,H>* c = static_cast<const Direct<G,H>*>(other);
   return cmp(*c);
  }

  virtual int cmp(const Direct<G,H>& other) const {
   int cmpLeft = _left.cmp(other._left);
   if (cmpLeft != 0) return cmpLeft;
   return _right.cmp(other._right);
  }

  virtual bool contains(const elem_t& x) const {
   return _left.contains(x.first) && _right.contains(x.second);
  }

  virtual int indexElem(const elem_t& x) const {
   if (contains(x))
    return _left.indexElem(x.first)*_right.order() + _right.indexElem(x.second);
   else throw group_mismatch("Direct::indexElem");
  }

/* TODO: Prove this is correct:
  virtual int exponent() const {
   return lcm(_left.exponent(), _right.exponent());
  }
*/

	G& left()        {return _left; }
  const G& left()  const {return _left; }
	H& right()       {return _right; }
  const H& right() const {return _right; }

  elem_t pair(const left_t& x, const right_t& y) const {
   if (_left.contains(x) && _right.contains(y)) return elem_t(x,y);
   else throw group_mismatch("Direct::pair");
  }

 private:
  G _left;
  H _right;
 };
}

#endif
