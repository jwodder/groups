/* Group utilities */

#include <functional>
#include <set>
#include "Groups/Grutils.hpp"
#include "Groups/Element.hpp"
#include "closure.hpp"

namespace Groups {
 Dicyclic* quaternion(int n) {return new Dicyclic(1 << (n-1)); }

 std::set<Element> closure(const std::set<Element>& start) {
  return closure2A<Element>(std::multiplies<Element>(), start.begin(),
			    start.end());
 }
}
