# -*- coding: utf-8 -*-
import itertools
import operator
import re
from   closure     import closure2A
from   permutation import Permutation

__all__ = ["Group", "Element",
	   "Cyclic", "Semidirect", "DirectProduct", "Dicyclic", "Quaternion",
	   "Dihedral", "Trivial", "Klein4", "AutCyclic", "HolCyclic",
	   "CycSemiCyc", "Symmetric",
	   "getGroup",
	   "closure", "isHomomorphism", "isHomomorphismFrom", "isClosed",
	   "commutators"]

class Metagroup(type):
    def __new__(mcs, name, bases, dict):
	groupParams = dict.get('groupParams', None)
	elemParams  = dict.get('elemParams',  None)
	if groupParams is None:
	    def badInit(self, *args):
		raise NotImplementedError('groupParams is not defined')
	    dict['__init__'] = badInit
	elif elemParams is None:
	    def badInit(self, *args):
		raise NotImplementedError('elemParams is not defined')
	    dict['__init__'] = badInit
	else:
	    def getParam(i): return property(lambda self: self._params[i])
	    # getParam is needed to get around the fact that the lambdas
	    # created by `for i in foo: x = lambda y: i` will end up returning
	    # the value of `i` as of the end of the execution of the enclosing
	    # function (__new__) rather than as of the lambdas' creations.
	    # Creating a new namespace via a function definition avoids this.
	    gpqty = len(groupParams)
	    init0 = dict.get('__init__', None)
	    def init(self, *args):
		if len(args) != gpqty:
		    raise TypeError('Constructor takes exactly %d argument%s'
				     % (gpqty, 's' if gpqty != 1 else ''))
		self._params = args
		if init0 is not None: init0(self, *args)
	    dict['__init__'] = init
	    for i,p in enumerate(groupParams): dict[p] = getParam(i)
	    epqty = len(elemParams)
	    def elemInit(self, args, group):
		if len(args) != epqty:
		    raise TypeError('Constructor takes exactly %d argument%s'
				     % (epqty, 's' if epqty != 1 else ''))
		self._params = tuple(args)
		self._group = group
	    elemDict = {"__init__": elemInit, "elemParams": elemParams}
	    for i,p in enumerate(elemParams): elemDict[p] = getParam(i)
	    dict['Element'] = type('Element', (Element,), elemDict)
	return type.__new__(mcs, name, bases, dict)


class Group(object):
    __metaclass__ = Metagroup

    groupParams = None
    elemParams  = None
    def identity(self):    raise NotImplementedError
    def oper(self,x,y):    raise NotImplementedError
    def invert(self,x):    raise NotImplementedError
    def order(self,x):     raise NotImplementedError
    def indexElem(self,x): raise NotImplementedError
    def LaTeX(self):       raise NotImplementedError
    def showElem(self,x):  raise NotImplementedError
    def showUElem(self,x): return uniexp(self.showElem(x))
    def LaTeXElem(self,x): raise NotImplementedError
    def __unicode__(self): return uniexp(str(self))

    # Subclasses also need to define __len__, __iter__, __str__, and
    # (optionally) __unicode__.

    @property
    def params(self): return self._params

    @property
    def family(self): return self.__class__.__name__

    def __cmp__(self, other):
	return cmp(type(self), type(other)) or cmp(self.params, other.params)

    def __contains__(self, x): return x.group == self  ### Use `is` instead?
    def __nonzero__(self): return len(self) > 1
    def __hash__(self): return hash((self.family, self.params))
    def __mul__(self, other): return DirectProduct(self, other)

    def __repr__(self):
	return self.family + '(' + ', '.join(map(repr, self.params)) + ')'

    def elem(self, *args): return self.Element(args, self)
    def elements(self): return list(iter(self))
    def copy(self): return self.__class__(*self.params)
    def product(self, xs): return reduce(self.oper, xs, self.identity())

    def centralizer(self, elems):
	elems = list(elems)
	return filter(lambda x: all(x*y == y*x for y in elems), self)

    def center(self): return self.centralizer(self)

    def normalizer(self, elems):
	elems = set(elems)
	return filter(lambda x: all(x*y/x in elems for y in elems), self)

    def isNormal(self, elems):
	# whether `elems` is actually a subgroup is not checked
	elems = set(elems)
	return all(x*y/x in elems for x in self for y in elems)

    def isSubgroup(self, elems):
	elems = set(elems)
	return bool(elems) \
	   and all(x in self for x in elems) \
	   and all(x*y in elems for x in elems for y in elems)

    def isAbelian(self): return all(x*y == y*x for x in self for y in self)

    def conjugacies(self):
	yield set([self.identity()])
	left = set(self)
	left.remove(self.identity())
	while left:
	    least = minimum(left)
	    cc = set(x * least / x for x in left)
	    yield cc
	    left -= cc

    def lowerCentral(self):
	whole = frozenset(self)
	h = whole
	while True:
	    yield h
	    h = frozenset(commutators(whole, h))

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


class Element(object):
    @property
    def params(self): return self._params

    @property
    def group(self): return self._group

    @property
    def order(self): return self.group.order(self)

    @property
    def index(self): return self.group.indexElem(self)

    def __mul__(self, y): return self.group.oper(self,y)
    def __invert__(self): return self.group.invert(self)
    def __div__(self, y): return self * ~y
    __truediv__ = __div__

    def __repr__(self):
	return '%s.Element(%r, %r)' \
		% (self.group.family, self.params, self.group)

    def __str__(self):     return self.group.showElem(self)
    def __unicode__(self): return self.group.showUElem(self)
    def LaTeX(self):       return self.group.LaTeXElem(self)

    def __cmp__(self, other):
    ### Should sorting be based on element indices instead of params?
	return cmp(type(self), type(other)) or \
	       cmp((self.group, self.params), (other.group, other.params))

    def __hash__(self): return self.index
     ### Should type and/or group information also be included when hashing?

    def __nonzero__(self): return self.params != self.group.identity().params

    def __pow__(self, n):
	order = self.order
	n %= order
	if n == 0: return self.group.identity()
	if order - n < order // 2:
	    n = order - n
	    x = ~self
	else:
	    x = self
	i=1
	while not (n & i):
	    x *= x
	    i <<= 1
	agg = x
	i <<= 1
	x *= x
	while i <= n:
	    if n & i: agg *= x
	    i <<= 1
	    x *= x
	return agg

    def cycle(self):
	yield self.group.identity()
	x = self
	while x: yield x; x *= self


class Cyclic(Group):
### Should steps be taken to ensure that n is always positive?
    groupParams = ('n',)
    elemParams  = ('i',)
    def identity(self):    return self.elem(0)
    def oper(self,x,y):    return self.elem((x.i + y.i) % self.n)
    def invert(self,x):    return self.elem(-x.i % self.n)
    def __len__(self):     return self.n
    def __iter__(self):    return (self.elem(i) for i in range(self.n))
    def order(self,x):     return cycOrd(self.n, x.i)
    def indexElem(self,x): return x.i
    def __str__(self):     return  'Z' + sub(self.n)
    def __unicode__(self): return u'ℤ' + subU(self.n)
    def LaTeX(self):       return r'\mathbb{Z}' + sub(self.n)
    def showElem(self,x):  return shexp('x', x.i)
     ### TODO: Add an option for changing the name of the variable
    LaTeXElem = showElem
    def elem(self,i):      return self.Element((i % self.n,), self)


class Semidirect(Group):
    groupParams = ('g', 'h', 'phi')
    # It is the user's responsibility to ensure that phi is an actual valid
    # homomorphism from the Elements of h to the automorphism group on the
    # Elements of g.

    elemParams  = ('a', 'b')
    # Note that the parameters of the elements here are Element objects of the
    # component classes, not raw parameter tuples.  Will I regret this later?

    def identity(self): return self.elem(self.g.identity(), self.h.identity())
    def oper(self,x,y): return self.elem(x.a * self.phi(x.b)(y.a), x.b * y.b)
    def invert(self,x): return self.elem(self.phi(~x.b)(~x.a), ~x.b)
    def __len__(self):  return len(self.g) * len(self.h)
    def __iter__(self): return (self.elem(a,b) for a in self.g for b in self.h)
    def indexElem(self,x): return x.a.index * len(g) + x.b.index

    def order(self, x):
	# Should the results be cached somehow?
	i=1
	val=x
	while val.a and val.b:
	    val *= x
	    i += 1
	return i * (val.a or val.b).order

    def showElem(self,x):
	if x.a or x.b: return '(%s, %s)' % (x.a, x.b)
	else: return '1'

    def showUElem(self,x):
	if x.a or x.b: return u'(%s, %s)' % (unicode(x.a), unicode(x.b))
	# Testing seems to imply that the explicit calls to unicode() are
	# unnecessary when formatting a Unicode string, but I can't find this
	# behavior explicitly documented anywhere.
	else: return u'1'

    def LaTeXElem(self,x):
	if x.a or x.b: return '(%s, %s)' % (x.a.LaTeX(), x.b.LaTeX())
	else: return '1'

    ### TODO: Add an option for turning on "ab"-style showing

    def __str__(self): return showbinop(self.g, 'x|', self.h)
     ### Rethink the operator
    def __unicode__(self): return showbinopU(self.g, u'⋊', self.h)
    def LaTeX(self): return showbinop(self.g.LaTeX(), r'\rtimes', self.h.LaTeX())


class DirectProduct(Semidirect):
    groupParams = ('g', 'h')
    elemParams  = ('a', 'b')
    def oper(self, x, y):  return self.elem(x.a * y.a, x.b * y.b)
    def invert(self, x):   return self.elem(~x.a, ~x.b)
    def order(self,x):     return lcm(x.a.order, x.b.order)
    def __str__(self):     return showbinop(self.g,   '*', self.h)
    def __unicode__(self): return showbinopU(self.g, u'×', self.h)

    def LaTeX(self):
	return showbinop(self.g.LaTeX(), r'\times', self.h.LaTeX())

    # identity, indexElem, __len__, __iter__, showElem, showUElem, and
    # LaTeXElem are inherited from semidirect (though the last three might have
    # to be overridden if "ba"-style showing is ever implemented).

    @property
    def phi(self): return lambda y: lambda x: x


class Dicyclic(Group):
### Should steps be taken to ensure that n is always positive?
    groupParams = ('n',)
    elemParams  = ('i', 'j')

    def identity(self): return self.elem(0, False)

    def oper(self,x,y):
	return self.elem((x.i + (-y.i if x.j else y.i)
			      + (self.n if x.j and y.j else 0)) % (2*self.n),
			 x.j != y.j)

    def invert(self,x):
	return self.elem((x.i+self.n if x.j else -x.i) % (2*self.n), x.j)

    def __len__(self): return 4*self.n

    def __iter__(self):
	return (self.elem(i,j) for i in range(2*self.n) for j in [False, True])

    def order(self,x): return 4 if x.j else cycOrd(2*self.n, x.i)

    def indexElem(self,x): return (2*self.n if x.j else 0) + x.i

    def __str__(self): return 'Dic' + sub(self.n)

    def LaTeX(self): return r'\operatorname{Dic}' + sub(self.n)

    def showElem(self,x):
	(pre, i) = ('', x.i) if x.i < self.n else ('-', x.i - self.n)
	return pre + multish(shexp('i', i), 'j' if x.j else '1')

    LaTeXElem = showElem

def Quaternion(n=2):
    if n<2: raise ValueError('n must be at least 2')
    qn = Dicyclic(1 << (n-1))
    #qn.__str__ = qn.LaTeX = lambda self: 'Q' + sub(1 << (n+1))
     ### TODO: Figure out how to make this work.
    return qn
    ### When n=2, the group should be somehow modified so that 'ij' is shown as
    ### 'k'.


class Dihedral(Group):
    groupParams = ('n',)   ### n must be positive.
    elemParams  = ('s', 'r')

    def identity(self): return self.elem(False, 0)
    def invert(self,x): return x if x.s else self.elem(False, -x.r % self.n)
    def order(self,x): return 2 if x.s else cycOrd(self.n, x.r)
    def indexElem(self,x): return (self.n if x.s else 0) + x.r
    def __len__(self): return 2*self.n
    def __str__(self): return 'Dih' + sub(self.n)
    def LaTeX(self): return r'\operatorname{Dih}' + sub(self.n)

    def oper(self,x,y):
	return self.elem(x.s != y.s, (y.r + (-x.r if y.s else x.r)) % self.n)

    def __iter__(self):
	return (self.elem(s,r) for s in [False, True] for r in range(self.n))

    def showElem(self,x):
	return multish('s' if x.s else '1', shexp('r', x.r))
    ### Should there be an option for showing the 'r' before the 's'?

    LaTeXElem = showElem


class Trivial(Group):
    groupParams = ()
    elemParams  = ()
    def identity(self):    return self.elem()
    def oper(self,x,y):    return x
    def invert(self,x):    return x
    def order(self,x):     return 1
    def __len__(self):     return 1
    def __iter__(self):    yield self.elem()
    def __str__(self):     return '1'
    def LaTeX(self):       return '1'
    def indexElem(self,x): return 0
    def showElem(self,x):  return '1'
    LaTeXElem = showElem


class Klein4(Group):
    groupParams = ()
    elemParams  = ('a', 'b')
    def identity(self):    return self.elem(False, False)
    def oper(self,x,y):    return self.elem(x.a != y.a, x.b != y.b)
    def invert(self,x):    return x
    def order(self,x):     return 2 if x else 1
    def __len__(self):     return 4
    def __iter__(self):    return (self.elem(a,b) for a in [False, True]
						  for b in [False, True])
    def __str__(self):     return 'V_4'
    def LaTeX(self):       return 'V_4'
    def indexElem(self,x): return int(x.a) * 2 + int(x.b)
    def showElem(self,x):  return multish('a' if x.a else '1',
					  'b' if x.b else '1')
    LaTeXElem = showElem


class AutCyclic(Group):  # formerly "MultiplicN"
    groupParams = ('n',)
    elemParams  = ('i',)

    def __init__(self, n):
	self._elems = [i for i in range(1,n+1) if gcd(n,i) == 1]
	self._indices = dict(zip(self._elems, range(n)))

    def identity(self):    return self.elem(1)
    def oper(self,x,y):    return self.elem((x.i * y.i) % self.n)
    def invert(self,x):    return self.elem(modInverse(x.i, self.n))
    def indexElem(self,x): return self._indices[x.i]
    def __len__(self):     return len(self._elems)
    def __iter__(self):    return (self.elem(i) for i in self._elems)

    def order(self,x):
    ### TODO: Try to find a more efficient way to calculate this.
	i=1
	val=x
	while val:
	    val *= x
	    i += 1
	return i

    ### TODO: Rethink these:
    def __str__(self):     return  'Z'          + sub(self.n)  + '^*'
    def __unicode__(self): return u'ℤ'          + subU(self.n) + u'ˣ'
    def LaTeX(self):       return r'\mathbb{Z}' + sub(self.n)  + r'^\times{}'
    def showElem(self,x):  return  '*'       + str(x.i)     if x else  '1'
    def showUElem(self,x): return u'⋅'       + unicode(x.i) if x else u'1'
    def LaTeXElem(self,x): return r'\cdot{}' + str(x.i)     if x else  '1'


def HolCyclic(n):
    if n<1: raise ValueError('n must be at least 1')
    g = Cyclic(n)
    h = AutCyclic(n)
    return Semidirect(g, h, lambda y: lambda x: g.elem((x.i * y.i) % n))
    ### TODO: Should __str__ etc. be overridden to show $\Hol(\Z_n)$?

def CycSemiCyc(n,m,i):
    if n < 1 or m < 1:
	raise ValueError('n and m must be positive')
    elif pow(i,m,n) != 1:
	raise ValueError('invalid homomorphism')
    else:
	g = Cyclic(n)
	h = Cyclic(m)
	return Semidirect(g, h, lambda y: lambda x: g.elem((x.i * i**y.i) % n))


class Symmetric(Group):
    groupParams = ('n',)  ### Must be a nonnegative integer
    elemParams  = ('s',)  # `s` is a Permutation object
    def identity(self):    return self.elem(Permutation())
    def oper(self,x,y):    return self.elem(x.s * y.s)
    def invert(self,x):    return self.elem(x.s.inverse)
    def __len__(self):     return factorial(self.n)
    def order(self,x):     return x.s.order
    def indexElem(self,x): return x.s.lehmer
    def __str__(self):     return 'S' + sub(self.n)
    LaTeX = __str__
    def showElem(self,x):  return str(x.s)
    def LaTeXElem(self,x): return str(x.s).replace(' ', r'\>')
    def __iter__(self):
	return itertools.imap(self.elem, Permutation.s_n(self.n))

def closure(iterable): return closure2A(operator.mul, iterable)
 # assumes the iterable is over Elements of a single Group
 # returns an iterator
 ### TODO: Should this be a method of Group instead of a function?
 ### TODO: Should this return a set?

def getGroup(iterable):
    """Returns the Group in which all of the elements of the iterable are
       contained; raises a `TypeError` if two or more elements are from
       different Groups and a `ValueError` if the iterable is empty"""
    group = None
    for x in iterable:
	if group is None:
	    group = x.group
	elif x not in group:
	    raise TypeError('Elements are from different Groups')
    if group is None:
	raise ValueError('no Elements supplied')
    else:
	return group

def isHomomorphism(phi, g, h):
    """Tests whether the callable object `phi` is a homomorphism from the group
       `g` to the group `h`"""
    return all(phi(x) in h for x in g) and isHomomorphismFrom(phi, g)

def isHomomorphismFrom(phi, g):
    """Tests whether the callable object `phi` is a homomorphism from the group
       `g` to an unspecified group.  This function is useful when `phi` is
       already known to return elements of a single group."""
    return all(phi(x)*phi(y) == phi(x*y) for x in group for y in group)

def isClosed(iterable):
    hset = set(iterable)
    return hset and all(x/y in hset for x in hset for y in hset)

def commutators(iterable1, iterable2):
    ### TODO: Should this return a set?
    aset = set(iterable1)
    bset = set(iterable2)
    return closure(~(y*x) * (x*y) for x in aset for y in bset)

# Internal functions: ---------------------------------------------------------

subs   = {0x30: u'₀', 0x31: u'₁', 0x32: u'₂', 0x33: u'₃', 0x34: u'₄',
	  0x35: u'₅', 0x36: u'₆', 0x37: u'₇', 0x38: u'₈', 0x39: u'₉',
	  0x2d: u'₋'}

supers = {0x30: u'⁰', 0x31: u'¹', 0x32: u'²', 0x33: u'³', 0x34: u'⁴',
	  0x35: u'⁵', 0x36: u'⁶', 0x37: u'⁷', 0x38: u'⁸', 0x39: u'⁹',
	  0x2d: u'⁻'}

def sub(n):  n = str(n); return '_{' + n + '}' if len(n) > 1 else '_' + n
def subU(n): return unicode(n).translate(subs)

def sup(n):  n = str(n); return '^{' + n + '}' if len(n) > 1 else '^' + n
def supU(n): return unicode(n).translate(supers)

def uniexp(str):
    def mogrify(match):
	(cmd, val1, val2) = match.groups()
	return (val1 or val2).translate(subs if cmd == u'_' else supers)
    return re.sub(ur'([_^])(?:([-\d])|\{(-?\d*)\})', mogrify, unicode(str))

def multish(x,y):
    if x == '1': return y
    elif y == '1': return x
    else: return x + y

def shexp(x,i):
    if i == 0: return '1'
    elif i == 1: return x
    else: return x + sup(i)

def cycOrd(n,x): return n // gcd(x,n)

def showbinop(x,op,y):
    x = str(x)
    y = str(y)
    if ' ' in x: x = '(' + x + ')'
    if ' ' in y: y = '(' + y + ')'
    return x + ' ' + op + ' ' + y

def showbinopU(x,op,y):
    x = unicode(x)
    y = unicode(y)
    if u' ' in x: x = u'(' + x + u')'
    if u' ' in y: y = u'(' + y + u')'
    return x + u' ' + op + u' ' + y

def factorial(n):
    x=1
    for i in range(2,n+1):
	x *= i
    return x

def gcd(x,y):
    (a,b) = (abs(x), abs(y))
    if a == 0 and b == 0: return 0
    elif a == 0 or b == 0: return a or b
    while b != 0:
	(a,b) = (b, a % b)
    return a

def lcm(x,y): d = gcd(x,y); return 0 if d == 0 else abs(x*y) // d

def modInverse(a,n):
    (u, uc) = (abs(n), 0)
    (l, lc) = (a % u, 1)
    while l > 1:
	(u, uc, l, lc) = (l, lc, u % l, uc - lc * (u//l))
    if l == 1: return lc % abs(n)
    else: raise ValueError('%d has no multiplicative inverse modulo %d' % (a,n))
