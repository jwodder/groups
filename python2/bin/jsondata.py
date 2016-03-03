#!/usr/bin/python
import json
import re
import sys
sys.path.insert(1, sys.path[0] + '/..')
from groups.about import about
from groups.read  import readName

if len(sys.argv) < 2:
    raise SystemExit("Usage: %s group ...\n" % (sys.argv[0],))
sys.stdout.write('[\n');
first = True
for gname in sys.argv[1:]:
    g = readName(gname)
    if first:
        first = False
    else:
        sys.stdout.write(',\n')
    sys.stdout.write('\n')
    data = about(g)
    shown = json.dumps(data, sort_keys=True, indent=4, separators=(',', ': '))
    sys.stdout.write(re.sub('^', ' '*4, shown, flags=re.M))
sys.stdout.write('\n\n]\n')
