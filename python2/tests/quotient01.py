#!/usr/bin/python
import sys
sys.path.insert(1, sys.path[0] + '/..')
import groups
from groups.about import printAbout

z4 = groups.Cyclic(4)
g1 = groups.Semidirect(z4, z4, lambda y: lambda x: z4.invert(x) if y % 2 else x)
gn = groups.Quotient(g1, g1.generate([(2,2)]))  # isomorphic to Q_8
printAbout(gn)
print
