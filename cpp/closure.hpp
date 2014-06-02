#ifndef CLOSURE_H
#define CLOSURE_H

#include <list>
#include <queue>
#include <set>

/* `Iter` is an iterator over values of `T`.
 * `Func` is a type that can be called with two `T` arguments to obtain another
 *     `T` value.
 */
template<class T, class Func, class Iter>
std::set<T> closure2(Func f, Iter first, Iter last) {
 std::set<T> seen(first, last);
 std::list<T> nova(seen.begin(), seen.end());
 while (!nova.empty()) {
  std::list<T> newer;
  typename std::list<T>::const_iterator novaI;
  for (novaI = nova.begin(); novaI != nova.end(); novaI++) {
   const std::set<T> seenCopy(seen);
    /* Because iterating over `seen` while it's being modified is a bad idea */
   typename std::set<T>::const_iterator seenI;
   for (seenI = seenCopy.begin(); seenI != seenCopy.end(); seenI++) {
    T val1 = f(*seenI, *novaI);
    if (seen.insert(val1).second) newer.push_back(val1);
    T val2 = f(*novaI, *seenI);
    if (seen.insert(val2).second) newer.push_back(val2);
   }
  }
  nova = newer;
 }
 return seen;
}

/* `Iter` is an iterator over values of `T`.
 * `Func` is a type that can be called with two `T` arguments to obtain another
 *     `T` value.  Its operation is assumed to be associative.
 */
template<class T, class Func, class Iter>
std::set<T> closure2A(Func f, Iter first, Iter last) {
 const std::set<T> start(first, last);
 std::set<T> seen(start);
 std::list<T> nova(seen.begin(), seen.end());
 while (!nova.empty()) {
  std::list<T> newer;
  typename std::list<T>::const_iterator novaI;
  for (novaI = nova.begin(); novaI != nova.end(); novaI++) {
   typename std::set<T>::const_iterator startI;
   for (startI = start.begin(); startI != start.end(); startI++) {
    T newVal = f(*startI, *novaI);
    if (seen.insert(newVal).second) newer.push_back(newVal);
   }
  }
  nova = newer;
 }
 return seen;
}

/* `Iter` is an iterator over values of `T`.
 * `U` is a type with `begin` and `end` methods that return iterators of type
 *     `U::iterator` that iterate over values of `T`.  Alternatively, in C++11
 *     or later, it is any type that can be iterated over with a range-based
 *     `for` loop to produce `T` values.
 * `Func` is a type that can be called with a single `T` argument to obtain a
 *     `U` value.
 */
template<class T, class Func, class Iter, class U>
std::set<T> closure1m(Func f, Iter first, Iter last) {
 std::set<T> seen(first, last);
 std::queue<T> nova(seen.begin(), seen.end());
 while (!nova.empty()) {
  U newVals = f(nova.front());
  nova.pop();
#if defined(__cplusplus) && __cplusplus >= 201103L
  for (U uval : newVals) {
   if (seen.insert(uval).second) nova.push(uval);
  }
#else
  typename U::iterator uiter;
  for (uiter = newVals.begin(); uiter != newVals.end(); uiter++) {
   if (seen.insert(*uiter).second) nova.push(*uiter);
  }
#endif
 }
 return seen;
}

#endif
