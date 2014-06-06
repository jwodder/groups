def isHomomorphism(phi, g, h):
    """Tests whether the callable object `phi` is a homomorphism from the group
       `g` to the group `h`"""
    return all(phi(x) in h for x in g) and isHomomorphismFrom(phi, g)

def isHomomorphismFrom(phi, g):
    """Tests whether the callable object `phi` is a homomorphism from the group
       `g` to an unspecified group.  This function is useful when `phi` is
       already known to return elements of a single group."""
    return all(phi(x)*phi(y) == phi(x*y) for x in group for y in group)

def centralizer(g, iterable):
    alist = list(iterable)
    return [x for x in g if all(x*a/x == a for a in alist)]

def center(g): return [x for x in g if all(x*a/x == a for a in g)]

def normalizer(g, iterable):
    aset = set(iterable)
    return [x for x in g if all(x*a/x in aset for a in aset)]

def isNormal(g, iterable):
    aset = set(iterable)
    return all(x*a/x in aset for x in g for a in aset)

def isSubgroup(iterable):
    hset = set(iterable)
    return all(x/y in hset for x in hset for y in hset)

def commutators(iterable1, iterable2):
    aset = set(iterable1)
    bset = set(iterable2)
    return closure(~(y*x) * (x*y) for x in aset for y in bset)

def conjugacies(g):
    yield set([g.identity()])
    left = set(g)
    left.remove(g.identity())
    while left:
	least = minimum(left)
	cc = set(x * least / x for x in left)
	yield cc
	left -= cc

def nilpotence(g):
    if len(g) == 1: return 0
    def lowerCentrals():
	whole = set(g)
	h = whole
	while True:
	    yield h
	    h = commutators(whole, h)
    i = 1
    lc = lowerCentral()
    prev = lc.next()
    for h in lc:
	if h == prev: return None
	if len(h) == 1: return i
	i += 1
	prev = h

def isAbelian(g): return all(x*y == y*x for x in g for y in g)
