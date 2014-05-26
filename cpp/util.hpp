#ifndef GROUPS_UTIL_H
#define GROUPS_UTIL_H
#include <string>
#include <ostream>
namespace Groups {
 int gcd(int, int);
 int lcm(int, int);
 std::ostream& expgen(std::ostream&, const std::string&, int, const std::string&);
 int factorial(int);
}
#endif
