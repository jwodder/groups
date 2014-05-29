"""Permutations of finitely many positive integers"""

import operator

__all__ = ["Permutation"]

class Permutation(object):
    def __init__(self, mapping=(), even=None, order=None, lehmer=None):
    # not for public use
	self._map    = tuple(mapping)
	self._even   = even
	self._order  = order
	self._lehmer = lehmer
	i = len(self._map) - 1
	while i >= 0 and self._map[i] == i+1:
	    i -= 1
	self._map = self._map[0:i+1]

    @classmethod
    def identity(cls): return cls()

    def __call__(self, i):
	return self._map[i-1] if 0 < i <= len(self._map) else i

    def __mul__(self, other):
	return self.__class__((self(other(i+1))
			       for i in range(max(self.degree, other.degree))),
			      even = self._even == other._even
				  if self._even is not None
				      and other._even is not None
				  else None)

    #__rmul__ = __mul__

    def __repr__(self): return '%s(%r)' % (self.__class__.__name__, self._map)

    def __str__(self):
	cycles = self.toCycles()
	if cycles:
	    return ''.join('(' + ' '.join(map(str,cyc)) + ')' for cyc in cycles)
	else:
	    return '1'

    def __nonzero__(self): return self._map != ()

    def __cmp__(self, other):
	return cmp(type(self), type(other)) or cmp(self._map, other._map)

    def __hash__(self): return hash(self._map)

    @property
    def degree(self): return len(self._map)

    @property
    def inverse(self):
	newMap = [None] * len(self._map)
	for (a,b) in enumerate(self._map):
	    newMap[b-1] = a+1
	return self.__class__(newMap, even=self._even, order=self._order)

    @property
    def order(self):
	if self._order is None:
	    self._order = reduce(lcm, map(len, self.toCycles()), 1)
	return self._order

    @property
    def isEven(self):
	if self._even is None:
	    self._even = not sum((len(cyc)-1 for cyc in self.toCycles()), 0) % 2
	return self._even

    @property
    def isOdd(self): return not self.isEven()

    @property
    def sign(self): return 1 if self.isEven else -1

    @property
    def lehmer(self):
	if self._lehmer is None:
	    left = range(len(self._map), 0, -1)
	    self._lehmer = 0
	    for x in left[:]:
		i = left.index(self(x))
		del left[i]
		self._lehmer = self._lehmer * x + i
	return self._lehmer

    @classmethod
    def fromLehmer(cls, x):
	x0 = x
	code = []
	f = 1
	while x > 0:
	    code.append(x % f)
	    x /= f
	    f += 1
	mapping = []
	for (i,c) in enumerate(code):
	    mapping.insert(c, i+1)
	p = cls(reversed(mapping)).inverse
	p._lehmer = max(x0,0)
	return p

    def toCycles(self):
	used = [True] + [False] * len(self._map)
	cycles = []
	while True:
	    try:
		x = used.index(False, 1)
	    except ValueError:
		return cycles
	    cyke = [x]
	    used[x] = True
	    y = self(x)
	    while y != x:
		cyke.append(y)
		used[y] = True
		y = self(y)
	    if len(cyke) > 1:
		cycles.append(tuple(cyke))

    @classmethod
    def transposition(cls, a, b):
	"""Returns the permutation representing the transposition of the
	   positive integers `a` and `b`"""
	if a < 1 or b < 1:
	    raise ValueError('values must be positive')
	elif a == b:
	    return cls()
	else:
	    # For $a<b$, $Lehmer((a b)) = (b-a) (b-1)! + \sum_{i=a}^{b-2} i!$
	    big = max(a,b)
	    small = min(a,b)
	    lehmer = 0
	    fac = reduce(operator.mul, range(1, small+1))
	    for i in range(small, big-1):
	        lehmer += fac
		fac *= i+1
	    lehmer += fac * (big-small)
	    return cls((b if x == a else a if x == b else x
			for x in range(1, big+1)),
		       even=False, order=2, lehmer=lehmer)

    @classmethod
    def fromCycle(cls, cyc):
	cyc = list(cyc)
	if len(cyc) < 2: return cls()
	mapping = {}
	maxVal = 0
	for (i,v) in enumerate(cyc):
	    if v < 1: raise ValueError('values must be positive')
	    if v in mapping:
		raise ValueError('%s appears more than once in cycle' % (v,))
	    mapping[v] = cyc[i+1] if i < len(cyc)-1 else cyc[0]
	    if v > maxVal: maxVal = v
	return cls((mapping.get(i,i) for i in range(1, maxVal+1)),
		   even = len(cyc) % 2, order=len(cyc))

    @classmethod
    def fromCycles(cls, cycles):
	return reduce(operator.mul, map(cls.fromCycle, cycles), cls())

    def disjoint(self, other):
	for (i,(a,b)) in enumerate(zip(self._map, other._map)):
	    if i+1 != a and i+1 != b: return False
	return True

    @classmethod
    def firstOfDegree(cls, n):
	"""Returns the first `Permutation` of degree ``n`` in modified Lehmer
	   code order.  If ``n`` is 0 or 1 (or anything less than 0), this is
	   the identity.  For higher degrees, this is
	   ``Permutation.transposition(n, n-1)``."""
	return cls.identity() if n < 2 else cls.transposition(n, n-1)

    def next(self):
	"""Returns the next `Permutation` in modified Lehmer code order"""
	if self.degree < 2:
	    return self.transposition(1,2)
	else:
	    lehmer2 = self._lehmer+1 if self._lehmer is not None else None
	    map2 = list(self._map)
	    for i in range(1, len(map2)):
		if map2[i] > map2[i-1]:
		    i2 = 0
		    while map2[i] <= map2[i2]:
			i2 += 1
		    map2[i], map2[i2] = map2[i2], map2[i]
		    map2[:i] = reversed(map2[:i])
		    return self.__class__(map2, lehmer=lehmer2)
	    return self.firstOfDegree(self.degree+1)

    def prev(self):
	"""Returns the previous `Permutation` in modified Lehmer code order.
	   If ``self`` is the identity (which has Lehmer code 0), a
	   `ValueError` is raised."""
	if self.degree < 2:
	    raise ValueError('cannot decrement identity')
	lehmer2 = self._lehmer-1 if self._lehmer is not None else None
	map2 = list(self._map)
	for i in range(1, len(map2)):
	    if map2[i] < map2[i-1]:
		i2 = 0
		while map2[i] >= map2[i2]:
		    i2 += 1
		map2[i], map2[i2] = map2[i2], map2[i]
		map2[:i] = reversed(map2[:i])
		return self.__class__(map2, lehmer=lehmer2)


def gcd(x,y):
    (a,b) = (abs(x), abs(y))
    if a == 0 and b == 0: return 0
    elif a == 0 or b == 0: return a or b
    while b != 0:
	(a,b) = (b, a % b)
    return a

def lcm(x,y): d = gcd(x,y); return 0 if d == 0 else abs(x*y) // d
