def invariants(g):
    if not g.isAbelian():
        raise ValueError('group must be abelian')
    while g:
        x = max(g, key=g.order)
        yield g.order(x)
        g = Quotient(g, g.generate([x]))
