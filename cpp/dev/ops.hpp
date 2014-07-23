template<class T, Iter>
set<T> centralizer(const basic_group<T>& g, Iter first, Iter last) {
 const vector<T> elemVec = g.elements();
 set<T> elems(elemVec.begin(), elemVec.end());
 for (; first != last; first++) {
  set<T>::iterator iter;
  for (iter = elems.begin(); iter != elems.end(); ) {
   if (g.oper(*first, *iter) != g.oper(*iter, *first)) elems.erase(iter++);
   else iter++;
  }
 }
 return elems;
}

template<class T>
set<T> center(const basic_group<T>& g) {
 const vector<T> elems = g.elements();
 return centralizer(g, elems.begin(), elems.end());
}

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
 const vector<T> gelems = g.elements();
 return all_of(gelems.begin(), gelems.end(), [&](const T& x) -> bool {
  return normalizes(g, x, elems);
 });
}

template<class T>
set<T> isSubgroup(const basic_group<T>& g, const set<T>& elems) {
 auto isMember = [&g](const T& x) -> bool {return g.contains(x); };
 // TODO: Look for a better way to accomplish the above.
 if (elems.empty() || !all_of(elems.begin(), elems.end(), isMember))
  return false;
 for (const T& x: elems) {
  for (const T& y: elems) {
   if (elems.count(g.oper(x,y)) == 0) return false;
  }
 }
 return true;
}

template<class T>
deque< set<T> > conjugacies(const basic_group<T>& g) {
 deque< set<T> > conjClasses{set<T>{g.identity()}};
 vector<bool> used(g.order(), false);
 used[0] = true;
 vector<T> elems = g.elements();
 for (size_t i=0; i<elems.size(); i++) {
  if (!used[i]) {
   const T& x = elems[i];
   set<T> cclass;
   for (const T& y: elems) {
    T z = g.conjugate(y,x);
    cclass.insert(z);
    used[g.indexElem(z)] = true;
   }
   conjClasses.push_back(cclass);
  }
 }
 return conjClasses;
}

template<class T>
set<T> vec2set(const vector<T>& vec) {return set<T>(vec.begin(), vec.end()); }

template<class T> class LowerCentralSeries {
public:
 LowerCentralSeries(const basic_group<T>& h)
  : g(h), whole(vec2set(h.elements())), contents(whole), _index(0) { }

 const set<T>& operator*() const {return contents; }

 LowerCentralSeries& operator++() {
  contents = commutators(g, whole, contents);
  _index++;
  return *this;
 }

 LowerCentralSeries operator++(int) {
  LowerCentralSeries tmp = *this;
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
 return LowerCentralSeries(g);
}

template<class T>
int nilpotence(const basic_group<T>& g) {
 if (g.order() == 1) return 0;
 LowerCentralSeries lc = lowerCentral(g);
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
  * Given two subsets of the group that are already closed under the group
  * operation (i.e., that are subgroups; this precondition is not checked),
  * subgroupUnion() computes the closure of their union as a set<T>.
  **/
template<class T>
set<T> subgroupUnion(const basic_group<T>& g, const set<T>& a, const set<T>& b)
{
 queue<T> nova(deque<T>(a.begin(), a.end()));
 set<T> seen = union(a,b);
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

template<class T>
set<T> cycle(const basic_group<T>& g, const T& x) {
 const T id = g.identity();
 set<T> cyke{id};
 T y = x;
 while (y != id) {
  cyke.insert(y);
  y = g.oper(y,x);
 }
 return cyke;
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
  subs = union(subs, nova);
 }
 return subs;
}

template<class T> using SetWGens = pair<set<T>, set< set<T> > >;

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
   //subgrs[sg.first] = union(subgrs[sg.first], sg.second);
   subgrs[sg.first].insert(sg.second);
  }
 }
 return subgrs;
}

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
     newGens.insert(union(difference(a, cyc), b));
    }
   }
   return make_pair(newSubgr, newGens);
  }
 }
 return SetWGens<T>();
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
int exponent(const basic_group<T>& g) {
 int ex = 1;
 for (const T& x: g.elements()) ex = lcm(ex, g.order(x));
 return ex;
}

/**
  * Returns a set of the left cosets of a subset of a group (whether it is an
  * actual subset is not checked)
  **/
template<class T>
set< set<T> > leftCosets(const basic_group<T>& g, const set<T>& elems) {
 set< set<T> > cosets;
 set<T> used;
 for (const T& x: g.elements()) {
  if (used.count(x) > 0) continue;
  set<T> c;
  for (const T& y: elems) {
   const T& z = g.oper(x,y);
   used.insert(z);
   c.insert(z);
  }
  cosets.insert(c);
 }
 return cosets;
}

/**
  * Returns a set of the right cosets of a subset of a group (whether it is an
  * actual subset is not checked)
  **/
template<class T>
set< set<T> > rightCosets(const basic_group<T>& g, const set<T>& elems) {
 set< set<T> > cosets;
 set<T> used;
 for (const T& x: g.elements()) {
  if (used.count(x) > 0) continue;
  set<T> c;
  for (const T& y: elems) {
   const T& z = g.oper(y,x);
   used.insert(z);
   c.insert(z);
  }
  cosets.insert(c);
 }
 return cosets;
}
