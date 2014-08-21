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

    def identity(self): return self.n.elementSet

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
