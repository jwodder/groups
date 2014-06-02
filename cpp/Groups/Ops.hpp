#ifndef GRUTILS_H
#define GRUTILS_H

#include <functional>
#include <set>
#include "Groups/Element.hpp"
#include "Groups/Families/Dicyclic.hpp"
#include "closure.hpp"

namespace Groups {
 Dicyclic* quaternion(int n = 2);

 std::set<Element> closure(const std::set<Element>&);

 template<class Iter>
 std::set<Element> closure(Iter first, Iter last) {
  return closure2A<Element>(std::multiplies<Element>(), first, last);
 }
}
#endif
