#ifndef GROUPS_UTIL_H
#define GROUPS_UTIL_H

#include <ostream>
#include <stdexcept>
#include <string>
#include <typeinfo>

namespace Groups {
 class group_mismatch : public std::logic_error {
 public:
  explicit group_mismatch(const std::string& m) : logic_error(m) { }
 };

 int gcd(int, int);
 int lcm(int, int);
 int factorial(int);

 std::ostream& expgen(std::ostream&, const std::string&, int, const std::string&);

 template<class T, class U>
 int cmpTypes(const T& t, const U& u) {
  const std::type_info &tType = typeid(t), &uType = typeid(u);
  return tType == uType ? 0 : tType.before(uType) ? -1 : 1;
  /* Note that the ordering used by `before` may differ between successive
   * program invocations. */
 }
}

#endif
