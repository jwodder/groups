#!/usr/bin/python
import sys
sys.path.insert(1, sys.path[0] + '/..')
from groups.read  import readName

for gname in sys.argv[1:]:
    g = readName(gname)
    sys.stdout.write(g.cayleyU().encode('utf-8'))
    #sys.stdout.write(g.cayley())
    sys.stdout.write('\n')
