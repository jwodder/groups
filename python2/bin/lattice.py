#!/usr/bin/python
"""Prints the subgroup lattice of a given group as a DOT file.  Normal
   subgroups are represented as filled circles, while non-normal subgroups are
   empty circles."""

# TODO: It seems that, if two adjacent subgraphs have no lines between them
# (e.g., the 8 and 6 subgraphs in the S_4 lattice), then `dot` will draw them
# at the same height/rank.  Try to keep this from happening.

import sys
sys.path.insert(1, sys.path[0] + '/..')
from groups.read import readName
from lattice     import lattice

if len(sys.argv) != 2:
    sys.stderr.write("Usage: %s group\n" % (sys.argv[0],))
    sys.exit(2)

g = readName(sys.argv[1])
subsSet = g.subgroups()
subsDex = dict((s,i) for (i,s) in enumerate(subsSet))
byOrder = {}
for (i,s) in enumerate(subsSet):
    byOrder.setdefault(len(s), []).append(s)

print 'graph {'
for order in sorted(byOrder):
    print ' subgraph order%d {' % (order,)
    print '  rank = "same"'
    for s in byOrder[order]:
	print '  s%d [shape = "point"%s]' % (subsDex[s],
					     '' if g.isNormal(s)
						else ', fillcolor = "white"')
    print ' }'
graph = lattice(subsSet)
for j in graph:
    for i in graph[j]:
	print ' s%d -- s%d' % (subsDex[j], subsDex[i])
print '}'
