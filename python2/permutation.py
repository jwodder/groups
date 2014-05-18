"""Permutations of finitely many positive integers"""

import operator
from   moremath import lcm

__all__ = ["Permutation"]

class Permutation(object):
    def __init__(self, mapping=()):
	self._map = tuple(mapping)
	self._lehmer = None
	self._order = None
	self._even = None
	i = len(self._map) - 1
	while i >= 0 and self._map[i] == i+1:
	    i -= 1
	self._map = self._map[0:i+1]

    def __getitem__(self, i):
	### Add support for slices?
	if 0 < i <= len(self._map):
	    return self._map[i-1]
	else:
	    return i

    def __mul__(self, other):
	return Permutation(self[other[i]]
			   for i in range(1, max(self.degree, other.degree)+1))

    __rmul__ = __mul__

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
	return Permutation(newMap)

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
    def lehmer(self):
	if self._lehmer is None:
	    code = []
	    left = range(1, len(self._map)+1)
	    for x in range(1, len(self._map)+1):
		i = left.index(self[x])
		del left[i]
		code.append(i)
	    code.reverse()
	    self._lehmer = 0
	    fac = 1
	    for (c,f) in zip(code, range(1, len(code)+1)):
		self._lehmer += c * fac
		fac *= f
	return self._lehmer

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
	    y = self[x]
	    while y != x:
		cyke.append(y)
		used[y] = True
		y = self[y]
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
	    return cls(b if x == a else a if x == b else x
		       for x in range(1, max(a,b)+1))

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
	return cls(mapping.get(i,i) for i in range(1, maxVal+1))

    @classmethod
    def fromCycles(cls, cycles):
	return reduce(operator.mul, map(cls.fromCycle, cycles), cls())
