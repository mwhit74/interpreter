#fragment start *
"""
A recursive descent parser for EBNF2

----------------------------------------------------------------------------
/* an EBNF defintion for Ferg's simplified EBNF: EBNF2

In EBNF2, "<expression>" indicates one or more iterations of "expression".

The tokenizer/lexer will tokenize according to these rules:
     identifier =  letter ( letter | digit | "_" ).
     terminal   = '"'  any_character* '"' | "'"  any_character* "'".
*/

language = "language"  "=" sequence "." {rule} EOF.

rule =  nonterminal  "=" sequence "."  .

sequence =  terminal           more_stuff
		|	non-terminal       more_stuff
		|	enclosed_sequence  more_stuff
		.

more_stuff =  "|" sequence | sequence | @.

enclosed_sequence =
		 | "("  <sequence>  ")"   /* exactly one */
		 | "["  <sequence>  "]"   /* zero or one */
		 | "{"  <sequence>  "}"   /* zero to n   */
		 | "<"  <sequence>  ">"   /* one  to n   */
		 .

----------------------------------------------------------------------------
language = rule {rule} .

rule =  identifier  "=" expression "."  .

expression =  term  { "|" term } .

term =  factor {factor} . /* term = one or more factors */

factor =  identifier
          | terminal
          | "("  expression  ")"   /* exactly one */
          | "["  expression  "]"   /* zero or one */
          | "{"  expression  "}"   /* zero to n   */
          | "<"  expression  ">"   /* one  to n   */
          .
----------------------------------------------------------------------------
"""
import ebnfLexer as lexer
from   ebnfSymbols import *
import types

verbose = True
class ParserError(Exception): pass
token = None

def dq(s): return '"%s"' %s

TERMINAL = STRING
NONTERMINAL = IDENTIFIER
indent = 0

def push(s):
	global indent
	indent += 1
	if verbose: print((" >"*indent) + " " + s)

def pop(s):
	global indent
	if verbose: print((" <"*indent) + " " + s)
	indent -= 1

#-------------------------------------------------------------------
#  decorator @track
#-------------------------------------------------------------------
def track(func):
	def newfunc():
		push(func.__name__)
		func()
		pop(func.__name__)
	return newfunc
	
				
#-------------------------------------------------------------------
#
#-------------------------------------------------------------------
def parse(sourceText):
	global lexer 
	# create a Lexer object & pass it the sourceText
	lexer.initialize(sourceText)
	getToken()
	language()

#-------------------------------------------------------------------
#
#-------------------------------------------------------------------
def getToken():
	global token 
	token  = lexer.get()
	if verbose: print ( ("  "*indent) + " * " + token.show())

#-------------------------------------------------------------------
#
#-------------------------------------------------------------------
def error(msg):
	token.abort(msg)


#--------------------------------------------------------
#                   token_starts_enclosed_sequence
#--------------------------------------------------------
def token_starts_enclosed_sequence():
	"""
	"""
	if token.cargo in ("(", "[", "{","<"): return True
	return False


#-------------------------------------------------------------------
#        foundOneOf
#-------------------------------------------------------------------
def foundOneOf(argTokenTypes):
	for argTokenType in argTokenTypes:
		print "foundOneOf", argTokenType, token.type
		if token.type == argTokenType:
			return True
	return False

#-------------------------------------------------------------------
#        found
#-------------------------------------------------------------------
def found(argTokenType):
	if token.type == argTokenType:
		return True
	return False

#-------------------------------------------------------------------
#       expect
#-------------------------------------------------------------------
def expect(*args):
	for argTokenType in args:
		if token.type == argTokenType:
			getToken()
			return True
	error("I was expecting to find "
		+ dq(str(args))
		+ " but I found " 
		+ token.show()
		)



#--------------------------------------------------------
#                   nonterminal
#--------------------------------------------------------
@track
def nonterminal():
	pass

#--------------------------------------------------------
#                   terminal
#--------------------------------------------------------
@track
def terminal():
	pass

#--------------------------------------------------------
#                   enclosed_sequence
#--------------------------------------------------------
@track
def enclosed_sequence():
	"""
	enclosed_sequence =
		| "("  <sequence>  ")"   /* exactly one */
		| "["  <sequence>  "]"   /* zero or one */
		| "{"  <sequence>  "}"   /* zero to n   */
		| "<"  <sequence>  ">"   /* one  to n   */
		.
	"""
	if  found("("):
		sequence()
		expect(")")

	elif found("["):
		sequence()
		expect("]")

	elif found("{"):
		sequence()
		expect("}")

	elif found("<"):
		sequence()
		expect(">")

	elif found("."):
		pass

	else:
		error("enclosed_sequence: syntax error")

#--------------------------------------------------------
#                   more_stuff
#--------------------------------------------------------
@track
def more_stuff():
	"""
	more_stuff =  @ | "|" sequence | sequence .
	"""
	if str(token.cargo) in(">})]."):
		pass

	elif found("|"):
		sequence()
	else:
		sequence()

#--------------------------------------------------------
#                   sequence
#--------------------------------------------------------
@track
def sequence():
	"""
	sequence =  terminal           more_stuff
			|	non-terminal       more_stuff
			|	enclosed_sequence  more_stuff
			.
	"""
	if verbose: print "sequence"

	if found(TERMINAL):
		getToken()
		more_stuff()

	elif str(token.type) in ("{[(<"):
		getToken()

		enclosed_sequence()
		more_stuff()

	else:
		nonterminal()
		getToken()

		more_stuff()


#--------------------------------------------------------
#                   rule
#--------------------------------------------------------
@track
def rule():
	"""
	rule =  nonterminal  "=" sequence "."  .
	"""
	if verbose: print "rule"
	if found(NONTERMINAL):
		getToken()

		expect("=")
		sequence()
		expect(".")

	else:
		raise ParserError("rule: expecting nonterminal")


#--------------------------------------------------------
#                   language
#--------------------------------------------------------
@track
def language():
	"""
	language = "language"  "=" sequence "." {rule} EOF.
	"""

	expect("language")
	expect("=")
	sequence()
	expect(".")

	while not found(EOF):
		rule()

	expect(EOF)
	
