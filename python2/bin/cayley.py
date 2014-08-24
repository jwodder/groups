#!/usr/bin/python
import sys
sys.path.insert(1, sys.path[0] + '/..')
from groups.read import readName

if len(sys.argv) < 2 or (sys.argv[1] == '-a' and len(sys.argv) == 2):
    sys.stderr.write("Usage: %s [-a] group ...\n" % (sys.argv[0],))
    sys.exit(2)

if sys.argv[1] == '-a':
    def showTbl(g): return g.cayley()
    del sys.argv[1]
else:
    def showTbl(g): return g.cayleyU().encode('utf-8')

for gname in sys.argv[1:]:
    g = readName(gname)
    sys.stdout.write(showTbl(g) + '\n')
