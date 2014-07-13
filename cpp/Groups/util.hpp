#ifndef GROUPS_UTIL_H
#define GROUPS_UTIL_H

#include <ostream>
#include <stdexcept>  /* logic_error */
#include <string>
#include <typeinfo>  /* typeid */
#include <utility>  /* pair */
#include <vector>

namespace Groups {
 class group_mismatch : public std::logic_error {
 public:
  explicit group_mismatch(const std::string& m) : logic_error(m) { }
 };

 const bool vecFT_array[2] = {false, true};

 const std::vector<bool> vecFT(vecFT_array+0, vecFT_array+2);

 int gcd(int, int);
 int lcm(int, int);
 int factorial(int);

 std::ostream& expgen(std::ostream&, const std::string&, int, const std::string&);

 std::vector<int> vecN(int);

 template<class T, class U>
 int cmpTypes(const T& t, const U& u) {
  const std::type_info &tType = typeid(t), &uType = typeid(u);
  return tType == uType ? 0 : tType.before(uType) ? -1 : 1;
  /* Note that the ordering used by `before` may differ between successive
   * program invocations. */
 }

 template<class T, class U>
 std::vector< std::pair<T,U> > cartesian(const std::vector<T>& vec1, const std::vector<U>& vec2) {
  if (vec1.empty() || vec2.empty()) return std::vector< std::pair<T,U> >(0);
  std::vector< std::pair<T,U> > vecout(vec1.size() * vec2.size(), make_pair(vec1[0], vec2[0]));
  typename std::vector< std::pair<T,U> >::iterator iter = vecout.begin();
  typename std::vector<T>::const_iterator iter1;
  for (iter1 = vec1.begin(); iter1 != vec1.end(); iter1++) {
   typename std::vector<U>::const_iterator iter2;
   for (iter2 = vec2.begin(); iter2 != vec2.end(); iter2++) {
    *iter = make_pair(*iter1, *iter2);
    iter++;
   }
  }
  return vecout;
 }
}

#endif
