/* Utilities which the user of the library should not touch */

#ifndef GROUPS_INTERNALS_H
#define GROUPS_INTERNALS_H

#include <ostream>
#include <string>
#include <typeinfo>  /* typeid */
#include <utility>  /* pair */
#include <vector>

#if defined(__cplusplus) && __cplusplus >= 201103L
#define CLAIMS_CPP11
#endif

namespace Groups {
#ifdef CLAIMS_CPP11
 const std::vector<bool> vecFT{false, true};
#else
 const bool vecFT_array[2] = {false, true};
 const std::vector<bool> vecFT(vecFT_array+0, vecFT_array+2);
#endif

 template<class T> struct cmp_with {
  virtual ~cmp_with() { }
  virtual int cmp(const T&) const = 0;
  bool operator==(const T& y) const {return cmp(y) == 0; }
  bool operator< (const T& y) const {return cmp(y) <  0; }
  bool operator> (const T& y) const {return cmp(y) >  0; }
  bool operator>=(const T& y) const {return cmp(y) >= 0; }
  bool operator<=(const T& y) const {return cmp(y) <= 0; }
  bool operator!=(const T& y) const {return cmp(y) != 0; }
 };

 int gcd(int, int);
 int lcm(int, int);
 int factorial(int);
 std::ostream& expgen(std::ostream&, const std::string&, int, const std::string&);
 std::vector<int> vecN(int);
 int modulo(int, int);

 template<class T, class U>
 int cmpTypes(const T& t, const U& u) {
  const std::type_info &tType = typeid(t), &uType = typeid(u);
  return tType == uType ? 0 : tType.before(uType) ? -1 : 1;
  /* Note that the ordering used by `before` may differ between successive
   * program invocations. */
 }

 template<class T, class U>
 std::vector< std::pair<T,U> > cartesian(const std::vector<T>& vec1,
					 const std::vector<U>& vec2) {
  if (vec1.empty() || vec2.empty()) return std::vector< std::pair<T,U> >(0);
  std::vector< std::pair<T,U> > vecout(vec1.size() * vec2.size(),
				       std::make_pair(vec1[0], vec2[0]));
  typename std::vector< std::pair<T,U> >::iterator iter = vecout.begin();
  for (const T& x: vec1) {
   for (const U& y: vec2) {
    *iter = std::make_pair(x,y);
    iter++;
   }
  }
  return vecout;
 }
}

#endif
