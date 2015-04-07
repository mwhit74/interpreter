#fragment start *
import nxxLexer   as     lexer
from   nxxSymbols import EOF

#-------------------------------------------------
# support for writing output to a file
#-------------------------------------------------
def writeln(*args):
	for arg in args:
		f.write(str(arg))
	f.write("\n")

#fragment start core
#-----------------------------------------------------------------------
#
#                    main
#
#-----------------------------------------------------------------------
def main(sourceText):
	global f
	f = open(outputFilename, "w")
	writeln("Here are the tokens returned by the lexer:")

	# create an instance of a lexer
	lexer.initialize(sourceText)

	#------------------------------------------------------------------
	# use the lexer.getlist() method repeatedly to get the tokens in
	# the sourceText. Then print the tokens.
	#------------------------------------------------------------------
	while True:
		token = lexer.get()
		writeln(token.show(True))
		if token.type == EOF: break
	f.close()
#fragment stop core


if __name__ == "__main__":
	outputFilename = "output\\nxxLexerDriver_output.txt"
	sourceFilename = "input\\nxx1.txt"
	sourceText = open(sourceFilename).read()
	main(sourceText)
	print(open(outputFilename).read())
