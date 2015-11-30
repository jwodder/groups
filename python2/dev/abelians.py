import itertools
from   operator import mul
from   ???      import factor

def abelians(n):
    """Returns an iterator over all abelian groups of order `n` paired with
       lists of their invariant factors"""
    if n < 1:
        return
    elif n == 1:
        yield (Trivial, [])
    else:
        vals = []
        for (p,k) in factor(n):
            vals.append([[p**i for i in part] for part in partitions(k)])
        for xs in itertools.product(*vals):
            xs2 = reduce(lambda a,b: itertools.starmap(mul, izip_longest(a, b, fillvalue=1)), xs)
            yield (reduce(Direct, map(Cyclic, xs2)), xs2)

def partitions(n):
    def gen(qty, mx):
        if qty == 0:
            yield ()
        else:
            for i in xrange(min(qty,mx), 0, -1):
                for xs in gen(qty-i, i):
                    yield (i,) + xs
    if n < 1:
        raise ValueError
    else:
        return gen(n,n)
