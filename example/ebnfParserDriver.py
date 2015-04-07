#fragment start *
import ebnfParser as parser

#-------------------------------------------------
# support for writing output to a file
#-------------------------------------------------
def writeln(*args):
	for arg in args:
		f.write(str(arg))
	f.write("\n")

if __name__ == "__main__":
	outputFilename = "..\\fragments\\ebnfParserDriver_output.txt"
	sourceFilename = "ebnfDemoSource.txt"
	sourceText = open(sourceFilename).read()
	parser.parse(sourceText)
	#print(open(outputFilename).read())
