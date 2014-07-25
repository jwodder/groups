# -*- coding: utf-8 -*-

class GroupReaderError(ValueError): pass

tokens = {"Dih": ('Dih', r'\Dih', 'D'),
	  "Dic": ('Dic', r'\Dic'),
	  "Z": ('Z', r'\Z', 'ℤ', 'C', r'\mathbb{Z}'),
	  "A": ('A',),
	  "S": ('S',),
	  "Q": ('Q',),
	  "V_4": ('V_4', 'V_{4}', 'V'),
	  '×': ('times', r'\times', '×', 'x', 'X', '*'),
	  '^×': ('^×', '^*', '^ˣ', r'^\rtimes', r'^\rtimes{}', r'^{\rtimes}', '^x', '^X'),
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
	        raise GroupReaderError('Invalid lexeme: %r' % (s[i:i+10],))
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
	    raise GroupReaderError('Invalid lexeme: %r' % (s[i:i+10],))
