#include <algorithm>  /* all_of */
#include <deque>
#include <map>
#include <queue>
#include <set>
#include <vector>
#include "Groups/BasicGroup.hpp"
#include "ops_int.hpp"
using std::all_of;
using std::deque;
using std::map;
using std::queue;
using std::set;
using std::vector;
using Groups::basic_group;

template<class T>
bool normalizes(const basic_group<T>& g, const T& x, const set<T>& elems) {
 // cf. `norms` in the Haskell version
 return all_of(elems.begin(), elems.end(), [&](const T& y) -> bool {
  return elems.count(g.conjugate(x,y)) > 0;
 });
}

template<class T>
set<T> normalizer(const basic_group<T>& g, const set<T>& elems) {
 set<T> newElems;
 for (const T& x: g.elements()) {
  if (normalizes(g, x, elems)) newElems.insert(newElems.end(), x);
 }
 return newElems;
}

template<class T>
bool isNormal(const basic_group<T>& g, const set<T>& elems) {
 // whether `elems` is actually a subgroup is not checked
 const vector<T>& gelems = g.elements();
 return all_of(gelems.begin(), gelems.end(), [&](const T& x) -> bool {
  return normalizes(g, x, elems);
 });
}

template<class T>
bool isSubgroup(const basic_group<T>& g, const set<T>& elems) {
 if (elems.empty() || !g.isSubset(elems)) return false;
 for (const T& x: elems) {
  for (const T& y: elems) {
   if (elems.count(g.oper(x,y)) == 0) return false;
  }
 }
 return true;
}

template<class T> class LowerCentralSeries {
public:
 LowerCentralSeries(const basic_group<T>& h)
  : g(h), whole(h.elementSet()), contents(whole), _index(0) { }

 const set<T>& operator*() const {return contents; }

 LowerCentralSeries<T>& operator++() {
  contents = commutators(g, whole, contents);
  _index++;
  return *this;
 }

 LowerCentralSeries<T> operator++(int) {
  LowerCentralSeries<T> tmp = *this;
  ++*this;
  return tmp;
 }

 int index() const {return _index; }

private:
 const basic_group<T>& g;
 const set<T> whole;
 set<T> contents;
 int _index;
};

template<class T>
LowerCentralSeries<T> lowerCentral(const basic_group<T>& g) {
 return LowerCentralSeries<T>(g);
}

template<class T>
int nilpotence(const basic_group<T>& g) {
 if (g.order() == 1) return 0;
 LowerCentralSeries<T> lc = lowerCentral(g);
 set<T> prev = *lc;
 for (;;) {
  const set<T>& h = *++lc;
  if (h == prev) return -1;
  if (h.size() == 1) return lc.index();
  prev = h;
 }
}

template<class T>
T commutator(const basic_group<T>& g, const T& x, const T& y) {
 return g.oper(g.invert(g.oper(y,x)), g.oper(x,y));
}

template<class T>
set<T> commutators(const basic_group<T>& g, const set<T>& a, const set<T>& b) {
 set<T> outset;
 for (const T& x: a) {
  for (const T& y: b) {
   outset.insert(commutator(g,x,y));
  }
 }
 return g.closure(outset);
}

/**
  * Given two subsets of a group that are already closed under the group
  * operation (i.e., that are subgroups; this precondition is not checked),
  * subgroupUnion() computes the closure of their union.
  **/
template<class T>
set<T> subgroupUnion(const basic_group<T>& g, const set<T>& a, const set<T>& b)
{
 queue<T> nova(deque<T>(a.begin(), a.end()));
 set<T> seen = set_union(a,b);
 while (!nova.empty()) {
  T x = nova.front();
  nova.pop();
  const set<T> seenCopy(seen);
  for (const T& y: seenCopy) {
   T z1 = g.oper(x,y);
   if (seen.insert(z1).second) nova.push(z1);
   T z2 = g.oper(y,x);
   if (seen.insert(z2).second) nova.push(z2);
  }
 }
 return seen;
}

/** Returns a `set` of all subgroups of the given group */
template<class T>
set< set<T> > subgroups(const basic_group<T>& g) {
 map<set<T>, T> cycles;
 set< set<T> > subs;
 for (const T& x: g.elements()) {
  set<T> cyc = cycle(g,x);
  if (subs.insert(cyc).second) cycles.insert(make_pair(cyc, x));
 }
 for (const pair<set<T>,T>& cycCg: cycles) {
  if (cycCg.first.size() == 1) continue;
  set< set<T> > nova;
  for (const set<T>& subgr: subs) {
   if (subgr.count(cycCg.second) == 0) {
    nova.insert(subgroupUnion(g, subgr, cycCg.first));
   }
  }
  update(subs, nova);
 }
 return subs;
}

/** Returns a `map` mapping subgroups to `set`s of (minimal?) generating `set`s
  */
template<class T>
map< set<T>, set< set<T> > > subgroupGens(const basic_group<T>& g) {
 map< set<T>, set< set<T> > > cycles;
 for (const T& x: g.elements()) {
  cycles[cycle(g,x)].insert(set<T>{x});
 }
 map< set<T>, set< set<T> > > subgrs(cycles);
 for (const SetWGens<T>& cycGs: cycles) {
  if (cycGs.first.size() == 1) continue;
  deque< SetWGens<T> > nova;
  for (const SetWGens<T>& subgrGens: subgrs) {
   SetWGens<T> newSub = addCycle(g, cycGs.first, cycGs.second,
				    subgrGens.first, subgrGens.second);
   if (!newSub.first.empty()) nova.push_back(newSub);
  }
  for (const SetWGens<T>& sg: nova) {
   update(subgrs[sg.first], sg.second);
  }
 }
 return subgrs;
}

/**
  * Tests whether the callable object `phi` is a homomorphism from the group
  * `g` to the group `h`
  **/
template<class Func, class T, class U>
bool isHomomorphism(const Func& phi, const basic_group<T>& g, const basic_group<U>& h) {
 auto isMember = [&h, &phi](const T& x) -> bool {return h.contains(phi(x)); };
 const vector<T>& gelems = g.elements();
 if (!all_of(gelems.begin(), gelems.end(), isMember)) return false;
 for (const T& x: gelems) {
  for (const T& y: gelems) {
   if (h.oper(phi(x), phi(y)) != phi(g.oper(x,y))) return false;
  }
 }
 return true;
}

template<class T>
set< set<T> > conjugacies(const basic_group<T>& g) {
 return partitionGroup(g, [&g](const T& x) -> set<T> {
  set<T> cclass;
  for (const T& y: g.elements()) cclass.insert(g.conjugate(y,x));
  return cclass;
 });
}

/**
  * Returns a set of the left cosets of a subset of a group (whether it is an
  * actual subset is not checked)
  **/
template<class T>
set< set<T> > leftCosets(const basic_group<T>& g, const set<T>& elems) {
 return partitionGroup(g, [&g, &elems](const T& x) -> set<T> {
  return g.oper(x, elems);
 });
}

/**
  * Returns a set of the right cosets of a subset of a group (whether it is an
  * actual subset is not checked)
  **/
template<class T>
set< set<T> > rightCosets(const basic_group<T>& g, const set<T>& elems) {
 return partitionGroup(g, [&g, &elems](const T& x) -> set<T> {
  return g.oper(elems, x);
 });
}

template<class T>
vector<vector<bool>> lattice(const vector<set<T>>& subgrs) {
 map<size_t, set<size_t>> byOrder;
 for (size_t i=0; i<subgrs.size(); i++) {
  byOrder[subgrs[i].size()].insert(i);
 }
 vector<vector<bool>> graph(subgrs.size(), vector<bool>(subgrs.size(), false));
 map<size_t, set<size_t>>::iterator iter;
 for (iter = byOrder.begin(); iter != byOrder.end(); iter++) {
  map<size_t, set<size_t>>::iterator supIter = iter;
  for (supIter++; supIter != byOrder.end(); supIter++) {
   if (supIter->first % iter->first == 0) {
    for (size_t i: iter->second) {
     for (size_t j: supIter->second) {
      if (isSubset(subgrs[i], subgrs[j])) {
       graph[i][j] = true;
       for (size_t k=0; k<subgrs.size(); k++) {
	if (graph[k][i]) graph[k][j] = false;
       }
      }
     }
    }
   }
  }
 }
 return graph;
}
