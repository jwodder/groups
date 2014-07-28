#!/usr/bin/python
# TO ADD:
#  - commutators?
#  - automorphisms that elements induce by conjugation?
#  - maximal subgroups?
#  - maximal subgroups of each subgroup?
#  - cosets of subgroups?
#  - abelianity of subgroups?
#  - normal subgroups within subgroups?
import sys
sys.path.insert(1, sys.path[0] + '/..')
from groups.read      import readName
from groups.internals import gcd

def main(args):
    if not args:
	sys.stderr.write("Usage: %s group...\n" % (sys.argv[0],))
	sys.exit(2)
    startObj(bracket='[')
    for gname in args:
	g = readName(gname)
	subgrGens   = g.subgroupGens()
	subgrKeys   = dict((h, tuple(sorted(h))) for h in subgrGens.iterkeys())
	subgrSorted = sorted(subgrKeys, key=lambda h: subgrKeys[h])
	subgrNames  = dict((h, 'subgr%02d' % (i,))
			   for (i,h) in enumerate(subgrSorted))

	def set2list(s): return [g.showElem(x) for x in sorted(s)]
	def nameSet(s):  return subgrNames[frozenset(s)]

	startObj()
	objItem('name', str(g))
	objItem('order', len(g))
	objItem('abelian', g.isAbelian())
	objItem('exponent', g.exponent())
	objItem('rank', min(len(gen) for gen in subgrGens[frozenset(g)]))
	objItem('center', nameSet(g.center()))

	startObj('elements')
	for (i,x) in enumerate(g):
	    objItem(g.showElem(x),
		    {"index":       i,
		     "order":       g.order(x),
		     "inverse":     g.showElem(g.invert(x)),
		     "cycle":       nameSet(g.cycle(x)),
		     "centralizer": nameSet(g.centralizer([x])),
		    })
	endObj()

	startObj('subgroups')
	for (i,h) in enumerate(subgrSorted):
	    objItem(subgrNames[h],
		    {"index":       i,
		     "elements":    map(g.showElem, subgrKeys[h]),
		     "normal":      g.isNormal(h),
		     "order":       len(h),
		     "centralizer": nameSet(g.centralizer(h)),
		     "normalizer":  nameSet(g.normalizer(h)),
		     "generators":  [map(g.showElem, gen) for gen in sorted(tuple(sorted(gen)) for gen in subgrGens[h])],
		    })
	endObj()

	startObj('conjugacy_classes', '[')
	for cclass in g.conjugacies():
	    listItem(set2list(cclass))
	endObj(']')

	lc = []
	for h in g.lowerCentral():
	    lc.append(nameSet(h))
	    if len(h) == 1:
		nilpotence = len(lc) - 1
		break
	    elif len(lc) > 1 and lc[-1] == lc[-2]:
		nilpotence = None
		break
	objItem('nilpotence', nilpotence)
	objItem('lower_central_series', lc)
	objItem('commutator_subgroup', lc[1] if len(lc)>1 else lc[0])

	if nilpotence is not None:
	    objItem('solvable', True)
	else:
	    subQtys = set()
	    for h in subgrGens:
		subQtys.add(len(h))
	    for i in range(2, len(g)):
		if len(g) % i == 0 and gcd(i, len(g) // i) == 1 \
		    and i not in subQtys:
		    objItem('solvable', False)
		    break
	    else:
		objItem('solvable', True)

	objItem('simple', len(g) != 1 and not any(g.isNormal(h) for h in subgrGens if len(h) not in (1, len(g))))

	endObj()
    endObj(']')
    sys.stdout.write('\n')


def jsonify(obj):
    if obj is None: return 'null'
    elif isinstance(obj, bool): return repr(obj).lower()
    elif isinstance(obj, (list, tuple)):
	return '[' + ', '.join(map(jsonify, obj)) + ']'
    elif isinstance(obj, dict):
	return '{' + ', '.join(jsonify(k) + ': ' + jsonify(obj[k])
			       for k in sorted(obj.keys())) + '}'
    elif isinstance(obj, (int, long)): return str(obj)
    else:
	obj = obj.replace("\\", "\\\\")
	obj = obj.replace('"', '\\"')
	obj = obj.replace("\r", "\\n")
	obj = obj.replace("\n", "\\n")
	obj = obj.replace("\t", "\\t")
	return '"' + obj + '"'

jsonStack = []

def startObj(name=None, bracket='{'):
    if jsonStack:
	if jsonStack[-1]:
	    sys.stdout.write(',')
	sys.stdout.write('\n')
    sys.stdout.write(' ' * len(jsonStack))
    if name is not None:
	sys.stdout.write(jsonify(name) + ': ')
    sys.stdout.write(bracket)
    jsonStack.append(0)

def objItem(name, val):
    if jsonStack[-1]:
	sys.stdout.write(',')
    sys.stdout.write('\n' + ' ' * len(jsonStack) + jsonify(name) + ': ' + jsonify(val))
    jsonStack[-1] += 1

def listItem(val):
    if jsonStack[-1]:
	sys.stdout.write(',')
    sys.stdout.write('\n' + ' ' * len(jsonStack) + jsonify(val))
    jsonStack[-1] += 1

def endObj(bracket='}'):
    if jsonStack[-1]:
	sys.stdout.write('\n' + ' ' * (len(jsonStack)-1))
    sys.stdout.write(bracket)
    jsonStack.pop()
    if jsonStack:
	jsonStack[-1] += 1

if __name__ == "__main__":
    main(sys.argv[1:])
