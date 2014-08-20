#ifndef SEMIDIRECT_HPP
#define SEMIDIRECT_HPP

#include <sstream>
#include <utility>  /* pair */
#include "Groups/BasicGroup.hpp"
#include "Groups/Util.hpp"
#include "Groups/internals.hpp"  /* cartesian */

namespace Groups {
 template<class G, class H, class Func>
 class Semidirect : public basic_group< std::pair<typename G::elem_t,
						  typename H::elem_t> >,
		    public cmp_with< Semidirect<G,H,Func> > {
 public:
  typedef typename G::elem_t left_t;
  typedef typename H::elem_t right_t;
  typedef std::pair<left_t, right_t> elem_t;  // Why is this necessary?

  Semidirect(const G& g, const H& h, const Func& f) : left(g), right(h), phi(f)
  { }

  virtual ~Semidirect() { }

  virtual elem_t oper(const elem_t& x, const elem_t& y) const {
   return elem_t(_left.oper(x.first, phi(x.second, y.first)),
		 _right.oper(x.second, y.second));
  }

  virtual elem_t identity() const {
   return elem_t(_left.identity(), _right.identity());
  }

  virtual std::vector<elem_t> elements() const {
   return cartesian(_left.elements(), _right.elements());
  }

  virtual elem_t invert(const elem_t& x) const {
   const U& binv = _right.invert(x.second);
   return elem_t(phi(binv, _left.invert(x.first)), binv);
  }

  virtual int order() const {return _left.order() * _right.order(); }

  virtual int order(const elem_t& x) const {
   // Should the results be cached somehow?
   int i = 1;
   elem_t val = x;
   while (val.first != _left.identity() && val.second != _right.identity()) {
    val = oper(val, x);
    i++;
   }
   if (val.first == _left.identity()) return i * _right.order(val.second);
   else return i * _left.order(val.first);
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
   return _left.isAbelian() && _right.isAbelian() && ??? ;
  }

  virtual Semidirect<G,H,Func>* copy() const {return new Semidirect(*this); }

  virtual int cmp(const basic_group<elem_t>* other) const {
   int ct = cmpTypes(*this, *other);
   if (ct != 0) return ct;
   const Semidirect<G,H,Func>* c
    = static_cast<const Semidirect<G,H,Func>*>(other);
   return cmp(*c);
  }

  virtual int cmp(const Semidirect<G,H>& other) const {
   int cmpLeft = _left.cmp(other._left);
   if (cmpLeft != 0) return cmpLeft;
   int cmpRight = _right.cmp(other._right);
   if (cmpRight != 0) return cmpRight;
   return _phi < other._phi ? -1 : _phi > other._phi ? 1 : 0;
  }

  virtual bool contains(const elem_t& x) const {
   return _left.contains(x.first) && _right.contains(x.second);
  }

  virtual int indexElem(const elem_t& x) const {
   if (contains(x))
    return _left.indexElem(x.first)*_right.order() + _right.indexElem(x.second);
   else throw group_mismatch("Semidirect::indexElem");
  }

	G& left()        {return _left; }
  const G& left()  const {return _left; }
	H& right()       {return _right; }
  const H& right() const {return _right; }

	Func& phi()       {return _phi; }
  const Func& phi() const {return _phi; }

  left_t phi(const right_t& y, const left_t& x) {return _phi(y)(x); }

  elem_t pair(const left_t& x, const right_t& y) const {
   if (_left.contains(x) && _right.contains(y)) return elem_t(x,y);
   else throw group_mismatch("Semidirect::pair");
  }

 private:
  G _left;
  H _right;
  Func _phi;
 };
}

#endif
