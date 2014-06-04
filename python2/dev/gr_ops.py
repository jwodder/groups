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
