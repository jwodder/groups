# This requires implementing `partitions` first.

# Also, it requires Python v.2.6+.

def abelians(n):
    """Returns an iterator over all abelian groups of order `n` paired with
       lists of their invariant factors"""
    if n < 1: return
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
