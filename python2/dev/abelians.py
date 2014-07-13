# This requires Python v.2.6+.

def abelians(n):
    """Returns an iterator over all abelian groups of order `n` paired with
       lists of their invariant factors"""
    if n < 1: return
    elif n == 1: yield (Trivial, [])
    else:
	vals = []
	for (p,k) in factor(n):
	    vals.append([[p**i for i in part] for part in partitions(k)])
	def mully(a,b):
	    if a is None: return b
	    elif b is None: return a
	    else: return a*b
	for xs in itertools.product(*vals):
	    xs2 = reduce(lambda a,b: map(mully, a, b), xs)
	    yield (reduce(DirectProduct, map(Cyclic, xs2)), xs2)

def partitions(n):
    def gen(qty, mx):
	if qty == 0:
	    yield []
	else:
	    for i in range(min(qty,mx), 0, -1):
		for xs in gen(qty-i, i):
		    yield [i] + xs
    if n < 1: raise ValueError
    else: return gen(n,n)
