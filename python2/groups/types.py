# -*- coding: utf-8 -*-
import itertools
from   closure     import closure2A, close2, view
from   permutation import Permutation
import internals as I

__all__ = [
    "group", "subgroup",
    "Cyclic", "AutCyclic", "HolCyclic", "CycSemiCyc",
    "Semidirect", "DirectProduct", "GDih", "Dihedral",
    "Quotient",
    "Dicyclic", "Quaternion",
    "Symmetric", "Alternating",
    "Trivial", "Klein4",
    "Group", "Element",
    "isHomomorphism",
    "direct",
    "lattice", "reverseLattice",
]

class group(object):
    paramNames = None

    def __init__(self, *args):
	if self.paramNames is None:
	    raise NotImplementedError('paramNames is not defined')
	qty = len(self.paramNames)
	if len(args) != qty:
	    raise TypeError('Constructor takes exactly %d argument%s'
			     % (qty, 's' if qty != 1 else ''))
	self.params = args
	for n,p in zip(self.paramNames, args):
	    setattr(self, n, p)

    def identity(self):    raise NotImplementedError
    def oper(self,x,y):    raise NotImplementedError
    def invert(self,x):    raise NotImplementedError
    def order(self,x):     raise NotImplementedError
    def LaTeX(self):       raise NotImplementedError
    def showElem(self,x):  raise NotImplementedError
    def LaTeXElem(self,x): raise NotImplementedError

    def showUElem(self,x): return I.uniexp(self.showElem(x))
    def __unicode__(self): return I.uniexp(str(self))

    # Subclasses also need to define __len__, __iter__, __contains__, __str__,
    # and (optionally) __unicode__.

    def __hash__(self): return hash((self.family, self.params))

    def __nonzero__(self): return len(self) > 1

    def __cmp__(self, other):
	return cmp(type(self), type(other)) or cmp(self.params, other.params)

    def __repr__(self):
	return self.family + '(' + ', '.join(map(repr, self.params)) + ')'

    def __copy__(self): return self.__class__(*self.params)

    copy = __copy__

    def __deepcopy__(self, memo):
	from copy import deepcopy
	return self.__class__(*(deepcopy(p, memo) for p in self.params))

    @property
    def supergroup(self): return self

    @property
    def family(self): return self.__class__.__name__

    def elements(self): return list(iter(self))

    def product(self, xs): return reduce(self.oper, xs, self.identity())

    def conjugate(self, y, x): return self.oper(self.oper(y,x), self.invert(y))

    def centralizer(self, elems):
	elems = list(elems)
	op = self.oper
	return filter(lambda x: all(op(x,y) == op(y,x) for y in elems), self)

    def center(self): return self.centralizer(self)

    def normalizer(self, elems):
	elems = set(elems)
	return filter(lambda x: all(self.conjugate(x,y) in elems for y in elems), self)

    def isNormal(self, elems):
	# whether `elems` is actually a subgroup is not checked
	if not isinstance(elems, subgroup):
	    elems = set(elems)
	return all(self.conjugate(x,y) in elems for x in self for y in elems)

    def isSubgroup(self, elems):
	if isinstance(elems, subgroup):
	    return self.supergroup == elems.supergroup \
	       and (not isinstance(self, subgroup)
		     or elems.elems <= self.elems)
	else:
	    elems = set(elems)
	    op = self.oper
	    return bool(elems) \
	       and all(x in self for x in elems) \
	       and all(op(x,y) in elems for x in elems for y in elems)

    def isAbelian(self):
	op = self.oper
	return all(op(x,y) == op(y,x) for x in self for y in self)

    def lowerCentral(self):
	whole = frozenset(self)
	h = whole
	while True:
	    yield h
	    h = frozenset(self.commutators(whole, h))

    def nilpotence(self):
	if len(self) == 1: return 0
	i = 1
	lc = self.lowerCentral()
	prev = lc.next()
	for h in lc:
	    if h == prev: return None
	    if len(h) == 1: return i
	    i += 1
	    prev = h

    def closure(self, iterable): return closure2A(self.oper, iterable)
	# assumes the iterable is over elements of `self`
	# returns an iterator
	### TODO: Should this return a set?

    def generate(self, iterable):
	# like `closure`, but returns a `subgroup` object
        return subgroup(self, self.closure(iterable), check=False)

    def commutator(self, x, y):
	return self.oper(self.invert(self.oper(y,x)), self.oper(x,y))

    def commutators(self, iterable1, iterable2):
	# assumes the iterables are over elements of `self`
	### TODO: Should this return a set?
	aset = set(iterable1)
	bset = set(iterable2)
	return self.closure(self.commutator(x,y) for x in aset for y in bset)

    def pow(self, x, n):
	order = self.order(x)
	n %= order
	if n == 0: return self.identity()
	if order - n < order // 2:
	    n = order - n
	    x = self.invert(x)
	i=1
	while not (n & i):
	    x = self.oper(x,x)
	    i <<= 1
	agg = x
	i <<= 1
	x = self.oper(x,x)
	while i <= n:
	    if n & i: agg = self.oper(agg, x)
	    i <<= 1
	    x = self.oper(x,x)
	return agg

    def cycle(self, x):
	yield self.identity()
	y = x
	while y != self.identity():
	    yield y
	    y = self.oper(y,x)

    def subgroupUnion(self, iterable1, iterable2):
	"""Given two subsets of the group that are already closed under the
	   group operation (i.e., that are subgroups; this precondition is not
	   checked), `subgroupUnion` computes the closure of their union as a
	   `frozenset`."""
	# `subgroups` and `subgroupGens` depend upon this function returning a
	# `frozenset`.
	f = close2(self.oper)
	new = list(iterable1)
	seen = set(iterable2)
	seen.update(new)
	while new:
	    new = list(view(f(seen.copy(), new), seen))
	return frozenset(seen)

    def subgroups(self):
	"""Returns a `set` of `frozenset`s representing all subgroups of the
	   group"""
	cycles = dict((frozenset(self.cycle(x)), x) for x in self)
	subs = set(cycles.keys())
	for (cyc, cg) in cycles.iteritems():
	    if len(cyc) == 1: continue
	    new = [self.subgroupUnion(subgr, cyc)
		   for subgr in subs if cg not in subgr]
	    subs.update(new)
	return subs

    def subgroupGens(self):
	"""Returns a `dict` mapping subgroups (as `frozenset`s) to `set`s of
	   (minimal?) generating `frozenset`s"""
	cycles = {}
	for x in self:
	    cyc = frozenset(self.cycle(x))
	    cycles.setdefault(cyc, set()).add(frozenset([x]))
	def addCycle(cyc, gs, subgr, gens):
	    ### TODO: Is there any reason for this to only check singletons?
	    ### What effect would checking whether the first element of `gs`
	    ### was a subset of `subgr` have on performance?
	    for cgSet in gs:
		if len(cgSet) == 1:
		    [cg] = cgSet
		    break
	    else:
		return None
	    if cg in subgr: return None
	    for h in gens:
		if len(h) == 1:
		    if h <= cyc: return None
		    else: break
	    return (self.subgroupUnion(subgr, cyc),
		    set((a - cyc) | b for a in gens for b in gs))
	for (cyc, gs) in cycles.items():  # `for` must iterate over a new list!
	    if len(cyc) == 1: continue
	    new = filter(None, (addCycle(cyc, gs, subgr, gens)
				for (subgr, gens) in cycles.iteritems()))
	    for (s,g) in new:
		cycles.setdefault(s, set()).update(g)
	return cycles

    def exponent(self): return reduce(I.lcm, (self.order(x) for x in self), 1)

    def conjugacies(self):
	return self._partition(lambda x: (self.conjugate(y,x) for y in self))

    def leftCosets(self, elems):
	"""Returns an iterator over the left cosets (as `frozenset`s) of a
	   subset `elems` of the group.  `elems` must be an iterable object;
	   whether it actually defines a subset of the group is not checked."""
	if not isinstance(elems, subgroup):
	    elems = set(elems)
	return self._partition(lambda x: (self.oper(x,y) for y in elems))

    def rightCosets(self, elems):
	"""Returns an iterator over the right cosets (as `frozenset`s) of a
	   subset `elems` of the group.  `elems` must be an iterable object;
	   whether it actually defines a subset of the group is not checked."""
	if not isinstance(elems, subgroup):
	    elems = set(elems)
	return self._partition(lambda x: (self.oper(y,x) for y in elems))

    def _partition(self, func):
	used = set()
	for x in self:
	    if x not in used:
		part = frozenset(func(x))
		yield part
		used.update(part)

    def cayley(self):
	"""Returns a `str` representation of the Cayley table of the group"""
	show = self.showElem
	fwidth = max(len(show(x)) for x in self)
	tblstr = ' ' * fwidth
	for y in self:
	    tblstr += '|%-*s' % (fwidth, show(y))
	tblstr = tblstr.rstrip()
	tblstr += '\n' + '-' * fwidth + ('|' + '-' * fwidth) * len(self) + '\n'
	for x in self:
	    tblstr += '%-*s' % (fwidth, show(x))
	    for y in self:
		tblstr += '|%-*s' % (fwidth, show(self.oper(x,y)))
	    tblstr = tblstr.rstrip()
	    tblstr += '\n'
	return tblstr

    def cayleyU(self):
	"""Returns a `unicode` representation of the Cayley table of the
	   group"""
	show = self.showUElem
	fwidth = max(len(show(x)) for x in self)
	tblstr = u' ' * fwidth
	for y in self:
	    tblstr += u'|%-*s' % (fwidth, show(y))
	tblstr = tblstr.rstrip()
	tblstr += u'\n' + u'-' * fwidth + (u'|' + u'-' * fwidth) * len(self)
	tblstr += u'\n'
	for x in self:
	    tblstr += u'%-*s' % (fwidth, show(x))
	    for y in self:
		tblstr += u'|%-*s' % (fwidth, show(self.oper(x,y)))
	    tblstr = tblstr.rstrip()
	    tblstr += u'\n'
	return tblstr


class subgroup(group):
    paramNames = ('supergr', 'elems')

    def __init__(self, supergr, elems, check=True):
	elems = frozenset(elems)
	if check and not supergr.isSubgroup(elems):
	    raise ValueError('arguments do not define a valid subgroup')
	supergr = supergr.supergroup
	super(subgroup, self).__init__(supergr, elems)

    @property
    def supergroup(self): return self.supergr

    def identity(self):       return self.supergroup.identity()
    def oper(self,x,y):       return self.supergroup.oper(x,y)
    def invert(self,x):       return self.supergroup.invert(x)
    def __len__(self):        return len(self.elems)
    def __iter__(self):       return iter(sorted(self.elems))
    def __contains__(self,x): return x in self.elems
    def order(self,x):        return self.supergroup.order(x)

    ### TODO: Rethink these three methods:
    def __str__(self):
	return '{' + ', '.join(map(self.showElem, self.elems)) + '}'

    def __unicode__(self):
	return '{' + ', '.join(map(self.showUElem, self.elems)) + '}'

    def LaTeX(self):
	return r'\{' + ', '.join(map(self.LaTeXElem, self.elems)) + r'\}'

    def showElem(self,x):     return self.supergroup.showElem(x)
    def showUElem(self,x):    return self.supergroup.showUElem(x)
    def LaTeXElem(self,x):    return self.supergroup.LaTeXElem(x)

    def isAbelian(self):
	return self.supergroup.isAbelian() or super(subgroup, self).isAbelian()


class Group(group):
    paramNames = ('group',)

    def __init__(self, rawGroup):
	if isinstance(rawGroup, Group):
	    rawGroup = rawGroup.group
	super(Group, self).__init__(rawGroup)

    def identity(self):    return Element(self.group.identity(), self)
    def oper(self,x,y):    return Element(self.group.oper(x.value,y.value),self)
    def invert(self,x):    return Element(self.group.invert(x.value), self)
    def order(self,x):     return self.group.order(x.value)
    def LaTeX(self):       return self.group.LaTeX()
    def showElem(self,x):  return self.group.showElem(x.value)
    def showUElem(self,x): return self.group.showUElem(x.value)
    def LaTeXElem(self,x): return self.group.LaTeXElem(x.value)
    def __len__(self):     return len(self.group)

    def __iter__(self):
	return itertools.imap(lambda x: Element(x, self), self.group)

    def __str__(self): return str(self.group)
    def __unicode__(self): return unicode(self.group)

    @property
    def family(self): return self.group.__class__.__name__

    def __contains__(self, x):
	return isinstance(x, Element) and x.group == self \
				      and x.value in self.group


class Element(object):
    def __init__(self, val, gr):
	self.value = val
	self.group = gr

    @property
    def order(self): return self.group.order(self)

    @property
    def rawGroup(self): return self.group.group

    def __mul__(self, y): return self.group.oper(self,y)
    def __invert__(self): return self.group.invert(self)
    def __div__(self, y): return self * ~y
    __truediv__ = __div__

    def __repr__(self):    return 'Element(%r, %r)' % (self.value, self.group)
    def __str__(self):     return self.group.showElem(self)
    def __unicode__(self): return self.group.showUElem(self)
    def LaTeX(self):       return self.group.LaTeXElem(self)

    def __cmp__(self, other):
	return cmp(type(self), type(other)) or \
	       cmp((self.group, self.value), (other.group, other.value))

    def __hash__(self): return hash((self.rawGroup, self.value))

    def __nonzero__(self): return self.value != self.group.identity().value

    def __copy__(self): return self.__class__(self.value, self.group)

    copy = __copy__

    def __deepcopy__(self, memo):
	from copy import deepcopy
	return self.__class__(deepcopy(self.value, memo),
			      deepcopy(self.group, memo))

    def __pow__(self, n):
	return Element(self.rawGroup.pow(self.value, n), self.group)

    def cycle(self):
	return itertools.imap(lambda x: Element(x, self.group),
			      self.rawGroup.cycle(self.value))


class Cyclic(group):
    paramNames = ('n',)

    def __init__(self, n):
	if n < 1: raise ValueError('n must be positive')
	super(Cyclic, self).__init__(n)

    def identity(self):    return 0
    def oper(self,x,y):    return (x + y) % self.n
    def invert(self,x):    return -x % self.n
    def __len__(self):     return self.n
    def __iter__(self):    return iter(range(self.n))
    def __contains__(self,x): return 0 <= x < self.n
    def order(self,x):     return I.cycOrd(self.n, x)
    def __str__(self):     return  'Z' + I.sub(self.n)
    def __unicode__(self): return u'ℤ' + I.subU(self.n)
    def LaTeX(self):       return r'\mathbb{Z}' + I.sub(self.n)
    def showElem(self,x):  return I.shexp('x', x)
     ### TODO: Add an option for changing the name of the variable
    LaTeXElem = showElem

    def exponent(self):  return self.n
    def isAbelian(self): return True


class Semidirect(group):
    paramNames = ('g', 'h', 'phi')
    # It is the user's responsibility to ensure that phi is an actual valid
    # homomorphism from the elements of h to the automorphism group on the
    # elements of g.

    ### v.2.6+: Use `namedtuple` for elements

    def identity(self): return (self.g.identity(), self.h.identity())

    def oper(self,x,y):
	return (self.g.oper(x[0] * self.phi(x[1])(y[0])),
		self.h.oper(x[1] * y[1]))

    def invert(self,x):
	return (self.phi(h.invert(x[1]))(g.invert(x[0])), h.invert(x[1]))

    def __len__(self):  return len(self.g) * len(self.h)

    def __iter__(self): return ((a,b) for a in self.g for b in self.h)

    def __contains__(self, x):
	return I.isPair(x) and x[0] in self.g and x[1] in self.h

    def order(self, x):
	# Should the results be cached somehow?
	i=1
	val=x
	while val[0] != self.g.identity() and val[1] != self.h.identity():
	    val = self.oper(val, x)
	    i += 1
	if val[0] == self.g.identity(): return i * self.h.order(val[1])
	else: return i * self.g.order(val[0])

    def showElem(self,x):
	if x == self.identity(): return '1'
	else: return '(%s, %s)' % (self.g.showElem(x[0]), self.h.showElem(x[1]))

    def showUElem(self,x):
	if x == self.identity(): return u'1'
	else: return u'(%s, %s)' % (self.g.showUElem(x[0]), self.h.showUElem(x[1]))

    def LaTeXElem(self,x):
	if x == self.identity(): return '1'
	else: return '(%s, %s)' % (self.g.LaTeXElem(x[0]), self.h.LaTeXElem(x[1]))

    ### TODO: Add an option for turning on "ab"-style showing

    def __str__(self): return I.showbinop(self.g, 'x|', self.h)
     ### TODO: Rethink the operator
    def __unicode__(self): return I.showbinopU(self.g, u'⋊', self.h)
    def LaTeX(self): return I.showbinop(self.g.LaTeX(), r'\rtimes', self.h.LaTeX())


class DirectProduct(Semidirect):
    def __init__(self, g, h):
	super(DirectProduct, self).__init__(g, h, lambda y: lambda x: x)

    def oper(self, x, y):  return (self.g.oper(x[0], y[0]),
				   self.h.oper(x[1], y[1]))

    def invert(self, x):   return (self.g.invert(x[0]), self.h.invert(x[1]))
    def order(self,x):     return I.lcm(self.g.order(x[0]), self.h.order(x[1]))
    def __str__(self):     return I.showbinop(self.g,   '*', self.h)
    def __unicode__(self): return I.showbinopU(self.g, u'×', self.h)
    def LaTeX(self):       return I.showbinop(self.g.LaTeX(), r'\times', self.h.LaTeX())

    # identity, __len__, __iter__, __contains__, showElem, showUElem, and
    # LaTeXElem are inherited from semidirect (though the last three might have
    # to be overridden if "ba"-style showing is ever implemented).

    ### TODO: Prove this is correct:
    #def exponent(self): return I.lcm(self.g.exponent(), self.h.exponent())

    def isAbelian(self): return self.g.isAbelian() and self.h.isAbelian()


class GDih(Semidirect):
    ### Should `h` be a "boolean" group instead?  Should the "bool" element
    ### come before the abelian element, as with Dihedral, or should Dihedral
    ### have its elements flipped?

    def __init__(self, g):
	if not g.isAbelian():
	    raise ValueError('argument must be abelian')
	super(GDih, self).__init__(g, Cyclic(2),
				   lambda y: lambda x: g.invert(x) if y else x)

    def invert(self,x): return x if x[1] else (self.g.invert(x[0]), 0)
    def order(self, x): return 2 if x[1] else self.g.order(x[0])

    def __str__(self): return 'Dih(%s)' % (str(self.g),)
    def __unicode__(self): return u'Dih(%s)' % (unicode(self.g),)
    def LaTeX(self): return r'\operatorname{Dih}(%s)' % (self.g.LaTeX(),)

    def exponent(self):  return I.lcm(2, self.g.exponent())

    ###def isAbelian(self): ### `g` is of the form E_{2^n} for some n


class Dicyclic(group):
    paramNames = ('n',)

    def __init__(self, n):
	if n < 2: raise ValueError('n must be at least 2')
	super(Dicyclic, self).__init__(n)

    ### v.2.6+: Use `namedtuple` for elements

    def identity(self): return (0, False)

    def oper(self,x,y):
	return ((x[0] + (-y[0] if x[1] else y[0])
		      + self.n * (x[1] and y[1])) % (2*self.n),
		x[1] != y[1])

    def invert(self,x):
	return ((x[0]+self.n if x[1] else -x[0]) % (2*self.n), x[1])

    def __len__(self): return 4*self.n

    def __iter__(self):
	return ((i,j) for i in range(2*self.n) for j in [False, True])

    def __contains__(self, x):
	return I.isPair(x) and 0 <= x[0] < 2*self.n and 0 <= x[1] < 2

    def order(self,x): return 4 if x[1] else I.cycOrd(2*self.n, x[0])

    def __str__(self): return 'Dic' + I.sub(self.n)

    def LaTeX(self): return r'\operatorname{Dic}' + I.sub(self.n)

    def showElem(self,x):
	(pre, i) = ('', x[0]) if x[0] < self.n else ('-', x[0] - self.n)
	return pre + I.multish(I.shexp('i', i), 'j' if x[1] else '1')

    LaTeXElem = showElem

    def exponent(self):  return 2*self.n
    def isAbelian(self): return False


class Quaternion(Dicyclic):
    def __init__(self, index=None, order=None):
	if index is not None:
	    if index < 2: raise ValueError('index must be at least 2')
	    n = 1 << (index-1)
	elif order is not None:
	    if order < 8: raise ValueError('order must be at least 8')
	    o = order
	    while not (o & 1):
		o >>= 1
	    if o != 1: raise ValueError('order must be a power of 2')
	    n = order >> 2
	else:
	    n = 2
	super(Quaternion, self).__init__(n)

    def __str__(self): return 'Q' + I.sub(len(self))

    LaTeX = __str__

    ### TODO: When n=2, `showElem` et alii should be modified so that 'ij' is
    ### shown as 'k'.


class Dihedral(group):
    paramNames = ('n',)

    def __init__(self, n):
	if n < 1: raise ValueError('n must be positive')
	super(Dihedral, self).__init__(n)

    ### v.2.6+: Use `namedtuple` for elements

    def identity(self): return (False, 0)
    def invert(self,x): return x if x[0] else (False, -x[1] % self.n)
    def order(self,x): return 2 if x[0] else I.cycOrd(self.n, x[1])
    def __len__(self): return 2*self.n
    def __str__(self): return 'Dih' + I.sub(self.n)
    def LaTeX(self): return r'\operatorname{Dih}' + I.sub(self.n)

    def oper(self,x,y):
	return (x[0] != y[0], (y[1] + (-x[1] if y[0] else x[1])) % self.n)

    def __iter__(self):
	return ((s,r) for s in [False, True] for r in range(self.n))

    def __contains__(self, x):
	return I.isPair(x) and 0 <= x[0] < 2 and 0 <= x[1] < self.n

    def showElem(self,x):
	return I.multish('s' if x[0] else '1', I.shexp('r', x[1]))
    ### Should there be an option for showing the 'r' before the 's'?

    LaTeXElem = showElem

    def exponent(self):  return I.lcm(2, self.n)
    def isAbelian(self): return self.n < 3


class Trivial(group):
    paramNames = ()
    def identity(self):    return ()
    def oper(self,x,y):    return x
    def invert(self,x):    return x
    def order(self,x):     return 1
    def __len__(self):     return 1
    def __iter__(self):    yield ()
    def __contains__(self, x): return x == ()
    def __str__(self):     return '1'
    def LaTeX(self):       return '1'
    def showElem(self,x):  return '1'
    LaTeXElem = showElem
    def exponent(self):    return 1
    def isAbelian(self):   return True


class Klein4(group):
    paramNames = ()
    ### v.2.6+: Use `namedtuple` for elements?
    def identity(self):    return (False, False)
    def oper(self,x,y):    return (x[0] != y[0], x[1] != y[1])
    def invert(self,x):    return x
    def order(self,x):     return 2 if any(x) else 1
    def __len__(self):     return 4
    def __iter__(self):    return ((a,b) for a in [False, True]
					 for b in [False, True])
    def __str__(self):     return 'V_4'
    def LaTeX(self):       return 'V_4'
    def showElem(self,x):  return I.multish('a' if x[0] else '1',
					    'b' if x[1] else '1')
    LaTeXElem = showElem

    def __contains__(self, x):
	return I.isPair(x) and x[0] in (0,1) and x[1] in (0,1)

    def exponent(self):    return 2
    def isAbelian(self):   return True


class AutCyclic(group):
    paramNames = ('n',)

    def __init__(self, n):
	if n < 1: raise ValueError('n must be positive')
	super(AutCyclic, self).__init__(n)
	self._elems = [i for i in range(1,n+1) if I.gcd(n,i) == 1]

    def identity(self):    return 1
    def oper(self,x,y):    return (x * y) % self.n
    def invert(self,x):    return I.modInverse(x, self.n)
    def __len__(self):     return len(self._elems)
    def __iter__(self):    return iter(self._elems)
    def __contains__(self, x): return 0 <= x < self.n and I.gcd(x, self.n) == 1

    def order(self,x):
    ### TODO: Try to find a more efficient way to calculate this.
	i=1
	val=x
	while val != 1:
	    val = (val * x) % self.n
	    i += 1
	return i

    ### TODO: Rethink these:
    def __str__(self):     return  'Z'          + I.sub(self.n)  + '^*'
    def __unicode__(self): return u'ℤ'          + I.subU(self.n) + u'ˣ'
    def LaTeX(self):       return r'\mathbb{Z}' + I.sub(self.n)  + r'^\times{}'
    def showElem(self,x):  return  '*'       + str(x)     if x else  '1'
    def showUElem(self,x): return u'⋅'       + unicode(x) if x else u'1'
    def LaTeXElem(self,x): return r'\cdot{}' + str(x)     if x else  '1'

    def isAbelian(self):   return True


def HolCyclic(n):
    if n<1: raise ValueError('n must be positive')
    g = Cyclic(n)
    h = AutCyclic(n)
    return Semidirect(g, h, lambda y: lambda x: g.elem((x * y) % n))
    ### TODO: Should __str__ etc. be overridden to show $\Hol(\Z_n)$?


def CycSemiCyc(n,m,i):
    if n < 1 or m < 1:
	raise ValueError('n and m must be positive')
    elif pow(i,m,n) != 1:
	raise ValueError('invalid homomorphism')
    else:
	g = Cyclic(n)
	h = Cyclic(m)
	return Semidirect(g, h, lambda y: lambda x: g.elem((x * i**y) % n))
	### TODO: Override __str__ etc. to show "Z_n \rtimes_i Z_m" (dropping
	### the "_i" when -1)


class Symmetric(group):
    paramNames = ('n',)

    def __init__(self, n):
	if n < 0: raise ValueError('n must be nonnegative')
	if n == 0: n = 1
	super(Symmetric, self).__init__(n)

    def identity(self):    return Permutation()
    def oper(self,x,y):    return x * y
    def invert(self,x):    return x.inverse
    def __len__(self):     return I.factorial(self.n)
    def order(self,x):     return x.order
    def __str__(self):     return 'S' + I.sub(self.n)
    LaTeX = __str__
    def showElem(self,x):  return str(x)
    def LaTeXElem(self,x): return str(x).replace(' ', r'\>')
    def __iter__(self):    return Permutation.s_n(self.n)

    def __contains__(self, x):
	return isinstance(x, Permutation) and x.degree <= self.n

    ### TODO: Prove this is correct:
    #def exponent(self): return reduce(I.lcm, xrange(1, self.n+1), 1)

    def isAbelian(self):   return self.n < 3


class Alternating(group):
    paramNames = ('n',)

    def __init__(self, n):
	if n < 0: raise ValueError('n must be nonnegative')
	if n == 0: n = 1
	super(Alternating, self).__init__(n)

    def identity(self):    return Permutation()
    def oper(self,x,y):    return x * y
    def invert(self,x):    return x.inverse
    def __len__(self):     return 1 if self.n == 1 else I.factorial(self.n) // 2
    def order(self,x):     return x.order
    def __str__(self):     return 'A' + I.sub(self.n)
    LaTeX = __str__
    def showElem(self,x):  return str(x)
    def LaTeXElem(self,x): return str(x).replace(' ', r'\>')

    def __iter__(self):
	return itertools.ifilter(lambda p: p.isEven, Permutation.s_n(self.n))

    def __contains__(self, x):
	return isinstance(x, Permutation) and x.degree <= self.n and x.isEven

    def isAbelian(self):   return self.n < 4


class Quotient(group):
    paramNames = ('g', 'n')

    def __init__(self, g, n, check=True):
	n = subgroup(g,n)
	if check and not g.isNormal(n):
	    raise ValueError('n must be a normal subgroup of g')
	super(Quotient, self).__init__(g,n)
	self._elems = list(self.leftCosets(n))
	self._toReps = dict((c, min(c)) for c in self._elems)
	self._fromReps = dict((x,c) for c in self._elems for x in c)

    def identity(self): return self.n.elems

    def oper(self, x, y):
	x = self._toReps[x]
	y = self._toReps[y]
	return self.fromReps[self.g.oper(x,y)]

    def invert(self, x):
	x = self._toReps[x]
	return self.fromReps[self.g.invert(x)]

    def order(self, x): return self.g.order(self._toReps[x])

    def __len__(self): return len(self._elems)

    def __iter__(self): return iter(self._elems)

    def __contains__(self, x): return x in self._toReps

    def __str__(self):
	return I.showbinop(self.g, '/', self.n)

    def __unicode__(self):
	return I.showbinopU(self.g, u'/', self.n)

    def LaTeX(self):
	return I.showbinop(self.g.LaTeX(), '/', self.n.LaTeX())

    ### TODO: Add a way to configure the representation of 'N'

    def showElem(self, x):
	return I.parenth(self.g.showElem(self._toReps[x])) + 'N'

    def showUElem(self, x):
	return I.parenth(self.g.showUElem(self._toReps[x])) + u'N'

    def LaTeXElem(self, x):
	return I.parenth(self.g.LaTeXElem(self._toReps[x])) + 'N'


def isHomomorphism(phi, g, h):
    """Tests whether the callable object `phi` is a homomorphism from the group
       `g` to the group `h`"""
    gop = g.oper
    hop = h.oper
    return all(phi(x) in h for x in g) \
       and all(hop(phi(x), phi(y)) == phi(gop(x,y)) for x in g for y in g)


def direct(g,h):
    if isinstance(g, Group) and isinstance(h, Group):
	return Group(DirectProduct(g.group, h.group))
    else:
	return DirectProduct(g,h)


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
	    out = graph.setdefault(s, set())
	     # So that every subgroup appears as a key of `graph`, even if its
	     # value is empty
	    for gn in subI:
		for g in byOrder[gn]:
		    if g.issubset(s):
			out.add(g)
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
