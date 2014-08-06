#ifndef TRIVIAL_HPP
#define TRIVIAL_HPP

#include "Groups/BasicGroup.hpp"
#include "Groups/internals.hpp"

namespace Groups {
 struct unit {
  int cmp(const unit&) const {return 0; }
  bool operator==(const unit&) const {return true; }
  bool operator< (const unit&) const {return false; }
  bool operator> (const unit&) const {return false; }
  bool operator>=(const unit&) const {return true; }
  bool operator<=(const unit&) const {return true; }
  bool operator!=(const unit&) const {return false; }
 };

 class Trivial : public basic_group<unit>, public cmp_with<Trivial> {
 public:
  Trivial() { }
  virtual ~Trivial() { }
  virtual unit oper(const unit&, const unit&) const {return unit(); }
  virtual unit identity() const {return unit(); }
  virtual std::vector<unit> elements() const {return std::vector<unit>(1); }
  virtual unit invert(const unit&) const {return unit(); }
  virtual int order() const {return 1; }
  virtual int order(const unit&) const {return 1; }
  virtual std::string showElem(const unit&) const {return "1"; }
  virtual bool isAbelian() const {return true; }
  virtual Trivial* copy() const {return new Trivial(); }
  virtual int cmp(const basic_group<unit>*) const {return 0; }
  virtual bool contains(const unit&) const {return true; }
  virtual int indexElem(const unit&) const {return 0; }
  virtual int cmp(const Trivial&) const {return 0; }
 };
}

#endif
