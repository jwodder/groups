def closure2(function,  iterable): return closureI(close2(function), iterable)
def closure2S(function, iterable): return closureS(close2(function), iterable)

def close2(function):
    def _close2(iter1, iter2):
	for a in iter1:
	    for b in iter2:
		yield function(a,b)
		yield function(b,a)
    return _close2

def closure2A(function, iterable):
    # For use when `function` is associative
    start = frozenset(iterable)
    def _close2A(old, new): return (function(y,x) for x in new for y in start)
    return closureI(_close2A, start)

def closure2AGens(function, generators):
    generators = frozenset(generators)
    #elems = {x: [x] for x in generators}
    elems = {}
    for x in generators: elems[x] = [x]
    newQueue = generators
    while newQueue:
	(oldQueue, newQueue) = (newQueue, [])
	for x in oldQueue:
	    for y in generators:
		z = function(x,y)
		if z not in elems:
		    elems[z] = elems[x] + [y]
		    newQueue.append(z)
    return elems

def closure1m(function, iterable):
    # `function` must have type `x -> iterable`.
    def _close1m(old, new): return (y for x in new for y in function(x))
    return closureI(_close1m, iterable)

def closure1mS(function, iterable):
    # `function` must have type `x -> iterable`.
    def _close1m(old, new): return (y for x in new for y in function(x))
    return closureS(_close1m, iterable)

def closureI(function, iterable):
    # `function` must have type `(iterable /* old */, iterable /* new */) ->
    # iterable`.
    seen = set(iterable)
    new = list(seen)
    for x in new:
	yield x
    while new:
	new = list(view(function(seen.copy(), new), seen))
	for x in new:
	    yield x

def closureS(function, iterable):
    # `function` must have type `(iterable /* old */, iterable /* new */) ->
    # iterable`.
    seen = set(iterable)
    new = list(seen)
    while new:
	new = list(view(function(seen.copy(), new), seen))
    return seen

def view(iterable, seen):
    for x in iterable:
	if x not in seen:
	    seen.add(x)
	    yield x

#def view(iterable, seen):
#    seen_add = seen.add  # Apparently this is a worthwhile optimization.
#    return (x for x in iterable if x not in seen and not seen_add(x))
