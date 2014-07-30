# TO ADD:
#  - commutators?
#  - automorphisms that elements induce by conjugation?
#  - maximal subgroups?
#  - maximal subgroups of each subgroup?
#  - cosets of subgroups?
#  - abelianity of subgroups?
#  - normal subgroups within subgroups?

import sys
from .types     import group
from .internals import gcd

__all__ = ["about", "printAbout"]

def about(g):
    subgrGens   = g.subgroupGens()
    subgrKeys   = dict((h, tuple(sorted(h))) for h in subgrGens.iterkeys())
    subgrSorted = sorted(subgrKeys, key=lambda h: subgrKeys[h])
    subgrNames  = dict((h, 'subgr%02d' % (i,))
		       for (i,h) in enumerate(subgrSorted))
    def set2list(s): return [g.showElem(x) for x in sorted(s)]
    def nameSet(s):  return subgrNames[frozenset(s)]
    lc = []
    for h in g.lowerCentral():
	lc.append(nameSet(h))
	if len(h) == 1:
	    nilpotence = len(lc) - 1
	    break
	elif len(lc) > 1 and lc[-1] == lc[-2]:
	    nilpotence = None
	    break
    if nilpotence is not None:
	solvable = True
    else:
	subQtys = set()
	for h in subgrGens:
	    subQtys.add(len(h))
	for i in range(2, len(g)):
	    if len(g) % i == 0 and gcd(i, len(g)//i) == 1 and i not in subQtys:
		solvable = False
		break
	else:
	    solvable = True
    return {
	"name":     str(g),
	"order":    len(g),
	"abelian":  g.isAbelian(),
	"exponent": g.exponent(),
	"rank":     min(len(gen) for gen in subgrGens[frozenset(g)]),
	"center":   nameSet(g.center()),
	"elements": dict((g.showElem(x),
			  {"index":       i,
			   "order":       g.order(x),
			   "inverse":     g.showElem(g.invert(x)),
			   "cycle":       nameSet(g.cycle(x)),
			   "centralizer": nameSet(g.centralizer([x])),
			  }) for (i,x) in enumerate(g)),
	"subgroups": dict((subgrNames[h],
			   {"index":       i,
			    "elements":    map(g.showElem, subgrKeys[h]),
			    "normal":      g.isNormal(h),
			    "order":       len(h),
			    "centralizer": nameSet(g.centralizer(h)),
			    "normalizer":  nameSet(g.normalizer(h)),
			    "generators":  [map(g.showElem, gen) for gen in sorted(tuple(sorted(gen)) for gen in subgrGens[h])],
			   }) for (i,h) in enumerate(subgrSorted)),
	"conjugacy_classes":    map(set2list, g.conjugacies()),
	"nilpotence":           nilpotence,
	"lower_central_series": lc,
	"commutator_subgroup":  lc[1] if len(lc)>1 else lc[0],
	"solvable":             solvable,
	"simple": len(g) != 1 and not any(g.isNormal(h)
					  for h in subgrGens
					  if len(h) not in (1, len(g))),
    }

aboutOrder = "name order abelian exponent rank center elements subgroups" \
	     " conjugacy_classes nilpotence lower_central_series" \
	     " commutator_subgroup solvable simple".split()

def printAbout(data, out=None, indent=0):
    if isinstance(data, group):
	data = about(data)
    if out is None:
	out = sys.stdout
    if isinstance(indent, int):
	indent = ' ' * indent
    out.write(indent + '{')
    indent += ' '
    first = True
    for key in aboutOrder:
	if first:
	    first = False
	else:
	    out.write(',')
	out.write('\n%s%s: ' % (indent, jsonify(key)))
	if key in ("elements", "subgroups"):
	    elems = sorted(data[key], key=lambda k: data[key][k]["index"])
	    out.write('{\n' + indent + ' ')
	    out.write((',\n' + indent + ' ').join(jsonify(k) + ': ' + jsonify(data[key][k]) for k in elems))
	    out.write('\n' + indent + '}')
	elif key == "conjugacy_classes":
	    out.write('[\n' + indent + ' ')
	    out.write((',\n' + indent + ' ').join(map(jsonify, data[key])))
	    out.write('\n' + indent + ']')
	else:
	    out.write(jsonify(data[key]))
    out.write('\n' + indent[:-1] + '}')

def jsonify(obj):
    if obj is None:
	return 'null'
    elif isinstance(obj, bool):
	return repr(obj).lower()
    elif isinstance(obj, (list, tuple)):
	return '[' + ', '.join(map(jsonify, obj)) + ']'
    elif isinstance(obj, dict):
	return '{' + ', '.join(jsonify(k) + ': ' + jsonify(obj[k])
			       for k in sorted(obj.keys())) + '}'
    elif isinstance(obj, (int, long)):
	return str(obj)
    else:
	obj = str(obj)
	obj = obj.replace("\\", "\\\\")
	obj = obj.replace('"', '\\"')
	obj = obj.replace("\r", "\\n")
	obj = obj.replace("\n", "\\n")
	obj = obj.replace("\t", "\\t")
	return '"' + obj + '"'
