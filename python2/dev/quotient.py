class Quotient(group):
    paramNames = ('dividend', 'divisor')

    def __init__(self, g, n):
	#if not isinstance(g, group):
	#    raise TypeError ###
	n = subgroup(g,n)
	if not g.isNormal(n):
	    raise ValueError ###
	super(Quotient, self).__init__(g,n)
	self._elems = list(self.leftCosets(n))
	self._toReps = dict((c, iter(c).next()) for c in self._elems)
	self._fromReps = dict((x,c) for c in self._elems for x in c)

    def identity(self): return self.divisor.elementSet

    def oper(self, x, y):
	x = self._toReps[x]
	y = self._toReps[y]
	return self.fromReps[self.dividend.oper(x,y)]

    def invert(self, x):
	x = self._toReps[x]
	return self.fromReps[self.dividend.invert(x)]

    def order(self, x): return self.dividend.order(self._toReps[x])

    def __len__(self): return len(self._elems)

    def __iter__(self): return iter(self._elems)

    def __contains__(self, x): return x in self._toReps

    __str__
    __unicode__
    LaTeX

    showElem
    showUElem
    LaTeXElem
