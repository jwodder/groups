# -*- coding: utf-8 -*-

__all__ = ["parse"]

def readGroup(s): return GroupReader().read(s)

class GroupReaderError(ValueError): pass

tokens = {"Dih": ('Dih', r'\Dih', r'\operatorname{Dih}', 'D'),
	  "Dic": ('Dic', r'\Dic', r'\operatorname{Dic}'),
	  "Z": ('Z', r'\Z', 'ℤ', 'C', r'\mathbb{Z}'),
	  "S": ('S',),
	  "A": ('A',),
	  "Q": ('Q',),
	  "V_4": ('V_4', 'V_{4}', 'V'),
	  '×': ('times', r'\times', '×', 'x', 'X', '*'),
	  '^×': ('^×', '^*', '^ˣ', '^x', '^X',
		 r'^\rtimes', r'^\rtimes{}', r'^{\rtimes}', '^rtimes'),
	  #'⋊': ('rtimes', r'\rtimes', 'x|'),
	  '_': ('_',),
	  '(': ('(',),
	  ')': (')',),
	 }

tokenTrie = dict()
for (token, lexemes) in tokens.iteritems():
    for lxm in lexemes:
	current = tokenTrie
	for c in lxm:
	    current = current.setdefault(c, dict())
	current[None] = token

def lex(s):
    i = 0
    while i < len(s):
	if s[i].isspace():
	    i += 1
	elif s[i] in tokenTrie:
	    j = i+1
	    stack = [tokenTrie[s[i]]]
	    while j < len(s) and s[j] in stack[-1]:
		stack.append(stack[-1][s[j]])
		j += 1
	    while stack and None not in stack[-1]:
		stack.pop()
		j -= 1
	    if stack:
		yield stack[-1][None]
		i = j
	    else:
		raise GroupReaderError('Invalid lexeme: %r' % (s[i:].split()[0],))
	elif s[i].isdigit():
	    j = i+1
	    while j < len(s) and s[j].isdigit(): j += 1
	    yield int(s[i:j])
	    i = j
	elif s[i] == '{':
	    i += 1
	    j = i
	    while j < len(s) and s[j].isdigit(): j += 1
	    if i == j or j == len(s) or s[j] != '}':
		raise GroupReaderError("'{' must be followed by a natural number and '}'")
	    yield int(s[i:j])
	    i = j+1
	else:
	    raise GroupReaderError('Invalid lexeme: %r' % (s[i:].split()[0],))

def quat(n):
    if n < 8:
	raise GroupReaderError('Quaternion subscript must be at least 8')
    i = 0
    while n > 1:
	n >>= 1
	i += 1
    if n /= 1:
	raise GroupReaderError('Quaternion subscript must be a power of 2')
    return Quaternion(i-1)

subscripted = {"Dih": Dihedral,
	       "Dic": Dicyclic,
	       "Z":   Cyclic,
	       "S":   Symmetric,
	       "A":   Alternating,
	       "Q":   quat}

class GroupReader(object):
    def __init__(self):
	self.state = GroupReader.beforeGroup
	self.stack = [[]]

    def read(self, s):
	for t in lex(s):
	    self.state = self.state(t)
	if self.state is not GroupReader.afterGroup:
	    raise GroupReaderError('Input ended in middle of parse')
	elif len(stack) > 1:
	    raise GroupReaderError('Unclosed parentheses')
	else:
	    return stack[-1][-1]

    def pushGroup(self, g):
	if stack[-1]:
	    stack[-1] = [DirectProduct(stack[-1][-1], g)]
	else:
	    stack[-1] = [g]

    def beforeGroup(self, t):  # The next token sequence must be a group or (
	if t == '(':
	    stack.append([])
	    return GroupReader.beforeGroup
	elif t == 'V_4':
	    self.pushGroup(Klein4())
	    return GroupReader.afterGroup
	elif t == 1:
	    self.pushGroup(Trivial())
	    return GroupReader.afterGroup
	elif t in subscripted:
	    self.classToken = t
	    return GroupReader.expectUnderscore
	else:
	    raise GroupReaderError('Expected group, got %r' % (t,))

    def expectUnderscore(self, t):
	if t == '_':
	    return GroupReader.expectSubscript
	else:
	    raise GroupReaderError('Underscore expected after %r' % (stack[-1][-1],))

    def expectSubscript(self, t):
	if isinstance(t, (int, long)):
	    self.pushGroup(subscripted[self.classToken](t))
	    if self.classToken == 'Z':
		return GroupReader.afterCyclic
	    else:
		return GroupReader.afterGroup
	else:
	    raise GroupReaderError('Number expected after subscript')

    def afterGroup(self, t):  # The next token sequence must be × or )
	if t == '×':
	    return Group.beforeGroup
	elif t == ')':
	    g = stack.pop()[0]
	    if stack:
		self.pushGroup(g)
		return Group.afterGroup
	    else:
		raise GroupReaderError('Too many closing parentheses')

    def afterCyclic(self, t):  # The next token sequence must be ^×, ×, or )
	if t == '^×':
	    stack[-1][-1] = AutCyclic(stack[-1][-1].n)
	else:
	    return self.afterGroup(t)
