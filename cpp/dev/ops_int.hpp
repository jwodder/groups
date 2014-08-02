/* Internal helper functions for ops.hpp */

#include <algorithm>
#include <iterator>  /* inserter */
#include <set>
#include <vector>
#include <utility>  /* pair, make_pair */
#include "Groups/BasicGroup.hpp"
using std::pair;
using std::set;
using std::vector;
using Groups::basic_group;

template<class T>
set<T> vec2set(const vector<T>& vec) {return set<T>(vec.begin(), vec.end()); }

template<class T> using SetWGens = pair<set<T>, set<set<T>>>;

template<class T>
SetWGens<T> addCycle(const basic_group<T>& g,
		     const set<T>& cyc,   const set< set<T> >& gs,
		     const set<T>& subgr, const set< set<T> >& gens) {
 /* TODO: Is there any reason for this to only check singletons?  What effect
  * would checking whether the first element of `gs` was a subset of `subgr`
  * have on performance? */
 for (const set<T>& cgSet: gs) {
  if (cgSet.size() == 1) {
   const T& cg = *cgSet.begin();
   if (subgr.count(cg) > 0) return SetWGens<T>();
   for (const set<T>& h: gens) {
    if (h.size() == 1) {
     if (cyc.count(*h.begin()) > 0) return SetWGens<T>();
     else break;
    }
   }
   set<T> newSubgr = subgroupUnion(g, subgr, cyc);
   set< set<T> > newGens;
   for (const set<T>& a: gens) {
    for (const set<T>& b: gs) {
     newGens.insert(set_union(difference(a, cyc), b));
    }
   }
   return std::make_pair(newSubgr, newGens);
  }
 }
 return SetWGens<T>();
}

template<class T>
set<T> set_union(const set<T>& a, const set<T>& b) {
 set<T> c;
 std::set_union(a.begin(), a.end(), b.begin(), b.end(), std::inserter(c, c.end()));
 return c;
}

/*
template<class T>
set<T> intersection(const set<T>& a, const set<T>& b) {
 set<T> c;
 std::set_intersection(a.begin(), a.end(), b.begin(), b.end(), std::inserter(c, c.end()));
 return c;
}
*/

template<class T>
set<T> difference(const set<T>& a, const set<T>& b) {
 set<T> c;
 std::set_difference(a.begin(), a.end(), b.begin(), b.end(), std::inserter(c, c.end()));
 return c;
}

template<class T>
void update(set<T>& a, const set<T>& b) {
 // TODO: Which of these two is faster?
 //a = set_union(a,b);
 a.insert(b.begin(), b.end());
}

template<class T, class Func>
set< set<T> > partitionGroup(const basic_group<T>& g, const Func& f) {
 set< set<T> > partitions;
 vector<bool> used(g.order(), false);
 const vector<T>& elems = g.elements();
 for (size_t i=0; i<elems.size(); i++) {
  if (!used[i]) {
   set<T> part = f(elems[i]);
   for (const T& y: part) used[g.indexElem(y)] = true;
   partitions.insert(part);
  }
 }
 return partitions;
}

/* Alternative implementation:
template<class T, class Func>
set< set<T> > partitionGroup(const basic_group<T>& g, const Func& f) {
 set< set<T> > partitions;
 set<T> used;
 for (const T& x: g.elements()) {
  if (used.count(x) == 0) {
   set<T> part = f(elems[i]);
   update(used, part);
   partitions.insert(part);
  }
 }
 return partitions;
}
*/
