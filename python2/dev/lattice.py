def lattice(subgrs):
    """Given an iterable of subgroups of a single group, `lattice` returns a
       `dict` in which each key `h` (a subgroup as a `frozenset`) is mapped to
       a `set` of `frozenset`s representing the maximal subgroups of `h` --
       i.e., the subgroups that `h` covers under the subgroup relation, i.e.,
       ``k in lattice(...)[h]`` iff `k` is a proper subgroup of `h` and there
       is no other proper subgroup of `h` that `k` is also a subgroup of."""
    byOrder = {}
    for s in subgrs:
	byOrder.setdefault(len(s), []).append(frozenset(s))
    graph = {}
    for i in sorted(byOrder, reverse=True):
	subI = [gn for gn in byOrder if gn < i and i % gn == 0]
	for s in byOrder[i]:
	    for gn in subI:
		for g in byOrder[gn]:
		    if g.issubset(s):
			graph.setdefault(s, set()).add(g)
			for hset in graph.itervalues():
			    if s in hset:
				hset.discard(g)
    return graph

def reverseLattice(d):
    if not isinstance(d, dict):
	d = lattice(d)
    d2 = {}
    for (a,bs) in d.items():
	for b in bs:
	    d2.setdefault(b, set()).add(a)
    return d2
