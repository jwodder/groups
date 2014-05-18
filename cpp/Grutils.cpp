/* Group utilities */

#include "Grutils.hpp"

namespace Groups {
 Dicyclic* quaternion(int n) {return new Dicyclic(1 << (n-1)); }
}
