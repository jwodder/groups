#ifndef KLEIN4_HPP
#define KLEIN4_HPP

#include <utility>  /* pair */
#include "Groups/BasicGroup.hpp"
#include "Groups/internals.hpp"

namespace Groups {
 class Klein4 : public basic_group< std::pair<bool,bool> >,
		public cmp_with<Klein4> {
 public:
  Klein4() { }
  virtual ~Klein4() { }
  virtual elem_t oper(const elem_t&, const elem_t&) const;
  virtual elem_t identity() const;
  virtual std::vector<elem_t> elements() const;
  virtual elem_t invert(const elem_t&) const;
  virtual int order() const;
  virtual int order(const elem_t&) const;
  virtual std::string showElem(const elem_t&) const;
  virtual bool isAbelian() const;
  virtual Klein4* copy() const;
  virtual int cmp(const basic_group<elem_t>*) const;
  virtual bool contains(const elem_t&) const;
  virtual int indexElem(const elem_t&) const;
  virtual int cmp(const Klein4&) const;
 };
}

#endif
