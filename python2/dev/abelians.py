# This requires implementing `partitions` first.

def abelians(n):
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
