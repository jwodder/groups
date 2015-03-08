#!/usr/bin/python
import sys
sys.path.insert(1, sys.path[0] + '/..')
from groups.read import readName

if len(sys.argv) < 2 or (sys.argv[1] == '-a' and len(sys.argv) == 2):
    raise SystemExit("Usage: %s [-a] group ...\n" % (sys.argv[0],))

if sys.argv[1] == '-a':
    def showTbl(g): return g.cayley()
    del sys.argv[1]
else:
    def showTbl(g): return g.cayleyU().encode('utf-8')

for gname in sys.argv[1:]:
    g = readName(gname)
    print showTbl(g)
