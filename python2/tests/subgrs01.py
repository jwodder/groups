import sys
sys.path.insert(1, sys.path[0] + '/..')
import groups

g = groups.Symmetric(4)
for h in sorted(tuple(sorted(h1)) for h1 in g.subgroups()):
# All of the sorting is needed for the output to be exactly the same as the
# Haskell version.
    print '{' + ', '.join(g.showElem(x) for x in h) + '}'
