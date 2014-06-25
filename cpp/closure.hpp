#ifndef CLOSURE_H
#define CLOSURE_H

#include <deque>
#include <queue>
#include <set>

/* `Iter` is an iterator over values of `T`.
 * `Func` is a type that can be called with two `T` arguments to obtain another
 *     `T` value.
 */
template<class T, class Func, class Iter>
std::set<T> closure2(const Func& f, Iter first, Iter last) {
 std::set<T> seen(first, last);
 std::queue<T> nova(std::deque<T>(first, last));
 while (!nova.empty()) {
  T n = nova.front();
  nova.pop();
  const std::set<T> seenCopy(seen);
   /* Because iterating over `seen` while it's being modified is a bad idea */
  typename std::set<T>::const_iterator seenI;
  for (seenI = seenCopy.begin(); seenI != seenCopy.end(); seenI++) {
   T val1 = f(*seenI, n);
   if (seen.insert(val1).second) nova.push(val1);
   T val2 = f(n, *seenI);
   if (seen.insert(val2).second) nova.push(val2);
  }
 }
 return seen;
}

/* `Iter` is an iterator over values of `T`.
 * `Func` is a type that can be called with two `T` arguments to obtain another
 *     `T` value.  Its operation is assumed to be associative.
 */
template<class T, class Func, class Iter>
std::set<T> closure2A(const Func& f, Iter first, Iter last) {
 const std::set<T> start(first, last);
 std::set<T> seen(start);
 std::queue<T> nova(std::deque<T>(first, last));
 while (!nova.empty()) {
  T n = nova.front();
  nova.pop();
  typename std::set<T>::const_iterator startI;
  for (startI = start.begin(); startI != start.end(); startI++) {
   T newVal = f(*startI, n);
   if (seen.insert(newVal).second) nova.push(newVal);
  }
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
std::set<T> closure1m(const Func& f, Iter first, Iter last) {
 std::set<T> seen(first, last);
 std::queue<T> nova(std::deque<T>(first, last));
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
