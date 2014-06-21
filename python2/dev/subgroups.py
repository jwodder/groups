from closure import close2, view

def subgroupUnion(self, iterable1, iterable2):  # `group` method
    """Given two subsets of the same group that are already closed under the
       group operation (i.e., that are subgroups; this precondition is not
       checked), `subgroupUnion` computes the closure of their union."""
    # `subgroups` and `subgroupGens` depend upon this function returning a
    # `frozenset`.
    f = close2(self.oper)
    new = list(iterable1)
    seen = set(iterable2)
    seen.update(new)
    while new:
	new = list(view(f(seen.copy(), new), seen))
    return frozenset(seen)


def subgroups(self):  # group method
    """Returns a `set` of `frozenset`s representing all subgroups of the
       group"""
    cycles = dict((frozenset(self.cycle(x)), x) for x in self)
    subs = set(cycles.keys())
    for (cyc, cg) in cycles.iteritems():
	new = [self.subgroupUnion(subgr, cyc)
	       for subgrs in subs if cg not in subgr]
	subs.update(new)
    return subs


# subgroupGens :: Group -> Map Subset (Set Subset)
def subgroupGens(self):  # group method
    """Returns a `dict` mapping subgroups to sets of (minimal?) generating
       sets"""
    cycles = {}
    for x in self:
	cyc = frozenset(self.cycle(x))
	cycles.setdefault(cyc, set()).add(frozenset([x]))
    def addCycle(cyc, gs, subgr, gens):
	#for cgSet in gs:
	#    if len(cgSet) == 1:
	#	[cg] = cgSet
	#	break
	#else:
	#    return None
	try:
	    [cg] = min(gs)
	except ValueError:
	    return None
	if cg in subgr: return None
	for h in gens:
	    if len(h) == 1:
		if h <= cyc: return None
		else: break
	return (self.subgroupUnion(subgr, cyc),
		set((a - cyc) | b for a in gens for b in gs))
    for (cyc, gs) in cycles.items():  # `for` must be iterating over a new list!
	new = filter(None, (addCycle(cyc, gs, subgr, gens)
			    for (subgr, gens) in cycles.iteritems()))
	for (s,g) in new:
	    cycles.setdefault(s, set()).update(g)
    return cycles
