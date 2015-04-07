#fragment start *
"""
A recursive descent parser in Python.

Adapted from: http://en.wikipedia.org/wiki/Recursive_descent_parser

The following EBNF-like grammar (for Niklaus Wirth's PL/0 programming language,
from Algorithms + Data Structures = Programs)
is in LL(1) form (for simplicity, IDENTIFIER and NUMBER are assumed to be terminals):

----------------------------------------------------------------------------

program = block "."
.

block =
	["const" IDENTIFIER "=" NUMBER {"," IDENTIFIER "=" NUMBER} ";"]
	["var" IDENTIFIER {"," IDENTIFIER} ";"]
	{"procedure" IDENTIFIER ";" block ";"} statement
.

statement =
	[IDENTIFIER ":=" expression
	| "call" IDENTIFIER
	| "begin" statement {";" statement} "end"
	| "if" condition "then" statement
	| "while" condition "do" statement
	]
.

condition =
	"odd" expression
	| expression ("="|"#"|"<"|"<="|">"|">=") expression
.

expression = ["+"|"-"] term {("+"|"-") term}
.

term = factor {("*"|"/") factor}
.

factor = IDENTIFIER | NUMBER | "(" expression ")"
.
----------------------------------------------------------------------------
Terminals are expressed in quotes (except for IDENTIFIER and NUMBER).
Each nonterminal is defined by a rule in the grammar.

Notice how closely the predictive parser below mirrors the grammar above.
There is a procedure for each nonterminal in the grammar.
Parsing descends in a top-down manner, until the final nonterminal has been processed.


The program fragment depends on:
 - a global variable 'token' which contains the next token from the input
 - the global function getToken() which updates 'token' when called.

'token' is an enumerated variable with the following valid values
which are determined by the programming language to be parsed.
"""

#fragment start snippet1
token = ""

ASSIGNMENT        = ":="
BEGIN             = "begin"
CALL              = "call"
COMMA             = ","
CONST             = "const"
DO                = "do"
END               = "end"
EQ                = "=="
GE                = ">="
GT                = ">"
IF                = "if"
LE                = "<="
LPAREN            = "("
LT                =  "<"
MINUS             = "-"
MULTIPLY          = "*"
NE                = "!="
ODD               = "odd"
PERIOD            = "."
PLUS              = "+"
PROC              = "proc"
RPAREN            = ")"
SEMICOLON         = ";"
SLASH             = "/"
THEN              = "then"
VAR               = "var"
WHILE             = "while"

IDENTIFIER        = "IDENTIFIER"
NUMBER            = "NUMBER"

class ParserException(Exception):
	pass

def error(msg):
	quotedToken = '"%s"' % token
	msg = msg + " while processing token " + quotedToken
	raise ParserException("\n\n" + msg)

def found(argToken):
	if (token == argToken):
		getToken()
		return True
	return False

def expect(argToken):
	if found(argToken):
		return  # no problem
	else:
		quotedToken = '"%s"' % argToken
		error("I was expecting to find token "
		+ quotedToken
		+ "\n  but I found something else" )

#--------------------------------------------------
def factor():
	"""
	factor = IDENTIFIER | NUMBER | "(" expression ")"
	.
	"""
	if found(IDENTIFIER):
		pass
	elif found(NUMBER):
		pass
	elif found(LPAREN):
		expression()
		expect(RPAREN)
	else:
		error("factor: syntax error")
		getToken()

#--------------------------------------------------
def term():
	"""
	term = factor {("*"|"/") factor}
	.
	"""
	factor()
	while found(MULTIPLY) or found(SLASH):
		factor()

#--------------------------------------------------
def expression():
	"""
	expression = ["+"|"-"] term {("+"|"-") term}
	.
	"""
	if found(PLUS) or found(MINUS):
		pass
	term()
	while found(PLUS) or found(MINUS):
		term()

#--------------------------------------------------
def condition():
	"""
	condition =
		"odd" expression
		| expression ("="|"#"|"<"|"<="|">"|">=") expression
	.
	"""
	if found(ODD):
		expression()
	else:
		expression()
		if (   found(EQ) or found(NE) or found(LT)
			or found(LE) or found(GT) or found(GE)  ):
			expression()
		else:
			error("condition: found invalid operator")
			getToken()


#--------------------------------------------------
def statement():
	"""
	statement =
		[IDENTIFIER ":=" expression
		| "call" IDENTIFIER
		| "begin" statement {";" statement} "end"
		| "if" condition "then" statement
		| "while" condition "do" statement
		]
	.
	"""
	if found(IDENTIFIER):
		expect(ASSIGNMENT)
		expression()

	elif found(CALL):
		expect(IDENTIFIER)

	elif found(BEGIN):
		statement()
		while found(SEMICOLON):
			statement()
		expect(END)

	elif found(IF):
		condition()
		expect(THEN)
		statement()

	elif found(WHILE):
		condition()
		expect(DO)
		statement()


#--------------------------------------------------
def block():
	"""
	block =
		["const" IDENTIFIER "=" NUMBER {"," IDENTIFIER "=" NUMBER} ";"]
		["var" IDENTIFIER {"," IDENTIFIER} ";"]
		{"procedure" IDENTIFIER ";" block ";"} statement
	.
	"""
	if found(CONST):
		expect(IDENTIFIER)
		expect(EQ)
		expect(NUMBER)

		while found(COMMA):
			expect(IDENTIFIER)
			expect(EQ)
			expect(NUMBER)
		expect(SEMICOLON)

	if found(VAR):
		expect(IDENTIFIER)
		while found(COMMA):
			expect(IDENTIFIER)
		expect(SEMICOLON)

	while found(PROC):
		expect(IDENTIFIER)
		expect(SEMICOLON)
		block()
		expect(SEMICOLON)

	statement()


#--------------------------------------------------
def program():
	"""
	program = block "."
	.
	"""
	getToken()
	block()
	expect(PERIOD)

#fragment stop snippet1



#----------------------------------------------------------
# the getToken() method to read the next token from
# the input stream of tokens
#----------------------------------------------------------
tokenIndex = -1
def getToken():
	global token,tokenIndex
	tokenIndex += 1
	try:
		token = tokens[tokenIndex]
	except IndexError:
		token = "EOF"
	print "token:", token


#----------------------------------------------------------
# Set up the input text
#----------------------------------------------------------
"""
block =
	["const" IDENTIFIER "=" NUMBER {"," IDENTIFIER "=" NUMBER} ";"]
	["var" IDENTIFIER {"," IDENTIFIER} ";"]
	{"procedure" IDENTIFIER ";" block ";"} statement
.
"""
pgm = """

proc IDENTIFIER ;
	var IDENTIFIER , IDENTIFIER ;
	IDENTIFIER := IDENTIFIER + IDENTIFIER ;

begin
call IDENTIFIER ;
end
.
"""
tokens = pgm.split()

#----------------------------------------------------------
# Now we run the parser
#----------------------------------------------------------
program()

