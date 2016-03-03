from   operator   import and_
from   .          import group, lattice
from   .internals import gcd

__all__ = ["about"]

def about(g):
    subgrGens   = g.subgroupGens()
    subgrGens[frozenset([g.identity()])].add(frozenset())
    subgrKeys   = dict((h, tuple(sorted(h))) for h in subgrGens.iterkeys())
    subgrSorted = sorted(subgrKeys, key=lambda h: subgrKeys[h])
    subgrNames  = dict((h, 'subgr%02d' % (i,))
                       for (i,h) in enumerate(subgrSorted))
    def set2list(s): return [g.showElem(x) for x in sorted(s)]
    def nameSet(s):  return subgrNames[frozenset(s)]
    graph = lattice(subgrGens.iterkeys())
    total = frozenset(g)
    lc = []
    for h in g.lowerCentral():
        lc.append(nameSet(h))
        if len(h) == 1:
            nilpotence = len(lc) - 1
            break
        elif len(lc) > 1 and lc[-1] == lc[-2]:
            nilpotence = None
            break
    if nilpotence is not None:
        solvable = True
    else:
        subQtys = set(len(h) for h in subgrGens)
        for i in range(2, len(g)):
            if len(g) % i == 0 and gcd(i, len(g)//i) == 1 and i not in subQtys:
                solvable = False
                break
        else:
            solvable = True
    return {
        "name":     str(g),
        "order":    len(g),
        "abelian":  g.isAbelian(),
        "exponent": g.exponent(),
        "rank":     min(len(gen) for gen in subgrGens[total]),
        "identity": g.showElem(g.identity()),
        "center":   nameSet(g.center()),
        "total_subgroup": nameSet(total),
        "elements": dict((g.showElem(x),
                          {"index":       i,
                           "order":       g.order(x),
                           "inverse":     g.showElem(g.invert(x)),
                           "cycle":       nameSet(g.cycle(x)),
                           "centralizer": nameSet(g.centralizer([x])),
                          }) for (i,x) in enumerate(g)),
        "subgroups": dict((subgrNames[h],
                           {"index":       i,
                            "elements":    map(g.showElem, subgrKeys[h]),
                            "normal":      g.isNormal(h),
                            "order":       len(h),
                            "centralizer": nameSet(g.centralizer(h)),
                            "normalizer":  nameSet(g.normalizer(h)),
                            "generators":  [map(g.showElem, gen) for gen in sorted(tuple(sorted(gen)) for gen in subgrGens[h])],
                            "maximal_subgroups": [nameSet(k) for k in sorted(graph[h], key=lambda k: subgrKeys[k])],
                           }) for (i,h) in enumerate(subgrSorted)),
        "conjugacy_classes":    map(set2list, g.conjugacies()),
        "nilpotence":           nilpotence,
        "lower_central_series": lc,
        "commutator_subgroup":  lc[1] if len(lc)>1 else lc[0],
        "solvable":             solvable,
        "simple": len(g) != 1 and not any(g.isNormal(h)
                                          for h in subgrGens
                                          if len(h) not in (1, len(g))),
        "Frattini_subgroup": nameSet(reduce(and_, graph[total]) if graph[total]
                                                                else total),
    }
