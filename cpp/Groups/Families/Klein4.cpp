#include <vector>
#include "Groups/Families/Klein4.hpp"
#include "Groups/internals.hpp"
using namespace std;

namespace Groups {
 typedef Klein4::elem_t elem_t;

 elem_t Klein4::oper(const elem_t& x, const elem_t& y) const {
  return elem_t(x.first ^ y.first, x.second ^ y.second);
 }

 elem_t Klein4::identity() const {return elem_t(false, false); }

 vector<elem_t> Klein4::elements() const {return cartesian(vecFT, vecFT); }

 elem_t Klein4::invert(const elem_t& x) const {return x; }

 int Klein4::order() const {return 4; }

 int Klein4::order(const elem_t& x) const {return x.first || x.second ? 2 : 1; }

 string Klein4::showElem(const elem_t& x) const {
  return x.first ? x.second ? "ab" : "a"
		 : x.second ?  "b" : "1";
 }

 bool Klein4::isAbelian() const {return true; }

 Klein4* Klein4::copy() const {return new Klein4(); }

 int Klein4::cmp(const basic_group<elem_t>* other) const {
  return cmpTypes(*this, *other);
 }

 int Klein4::cmp(const Klein4& other) const {return 0; }

 bool Klein4::contains(const elem_t& x) const {return true; }

 int Klein4::indexElem(const elem_t& x) const {return x.first * 2 + x.second; }

 int Klein4::exponent() const {return 2; }
}
