def lattice(subgrs):
    """Given an iterable of subgroups of a single group, `lattice` returns a
       `dict` in which each key `g` (a subgroup as a `frozenset`) is mapped to
       a `set` of `frozenset`s representing the subgroups that cover `g` under
       the subgroup relation -- i.e., ``h in lattice(...)[g]`` iff `g` is a
       proper subgroup of `h` and there is no other proper subgroup of `h` that
       `g` is also a subgroup of."""
    byOrder = {}
    for s in subgrs:
	byOrder.setdefault(len(s), []).append(frozenset(s))
    graph = {}
    for i in sorted(byOrder):
	superI = [gn for gn in byOrder if gn > i and gn % i == 0]
	for s in byOrder[i]:
	    for gn in superI:
		for g in byOrder[gn]:
		    if s.issubset(g):
			graph.setdefault(s, set()).add(g)
			for h in graph:
			    if s in graph[h]:
				graph[h].discard(g)
    return graph

def reverseLattice(d):
    if not isinstance(d, dict):
	d = lattice(d)
    d2 = {}
    for (a,bs) in d.items():
	for b in bs:
	    d2.setdefault(b, set()).add(a)
    return d2
