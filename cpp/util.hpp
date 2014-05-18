#ifndef GROUPS_UTIL_H
#define GROUPS_UTIL_H
#include <string>
#include <ostream>
namespace Groups {
 int gcd(int, int);
 int lcm(int, int);
 ostream& expgen(ostream&, const string&, int, const string&);
}
#endif
