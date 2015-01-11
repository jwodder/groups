#!/usr/bin/python
import sys
sys.path.insert(1, sys.path[0] + '/..')
from groups.read  import readName
from groups.about import printAbout

if len(sys.argv) < 2:
    sys.stderr.write("Usage: %s group ...\n" % (sys.argv[0],))
    sys.exit(2)
sys.stdout.write('[\n');
first = True
for gname in sys.argv[1:]:
    g = readName(gname)
    if first:
        first = False
    else:
        sys.stdout.write(',\n')
    sys.stdout.write('\n')
    printAbout(g, out=sys.stdout, indent=' ')
sys.stdout.write('\n\n]\n')
