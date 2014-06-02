#ifndef CLOSURE_H
#define CLOSURE_H

#include <list>
#include <queue>
#include <set>

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

/* For use when `f` is associative */
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

template<class T, class Func, class Iter, class U>
std::set<T> closure1m(Func f, Iter first, Iter last) {
 std::set<T> seen(first, last);
 std::queue<T> nova(seen.begin(), seen.end());
 while (!nova.empty()) {
  U newVals = f(nova.front());
  nova.pop();
  typename U::iterator uiter;
  for (uiter = newVals.begin(); uiter != newVals.end(); uiter++) {
   if (seen.insert(*uiter).second) nova.push(*uiter);
  }
 }
 return seen;
}

#endif
