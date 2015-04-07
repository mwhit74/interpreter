#fragment start *
import genericScanner as scanner

#-------------------------------------------
# support for writing output to a file
#-------------------------------------------
def writeln(*args):
	for arg in args:
		f.write(str(arg))
	f.write("\n")

#-----------------------------------------------------------------------
#
#                    main
#
#-----------------------------------------------------------------------
def main(sourceText):
	global f
	f = open(outputFilename, "w")      # open the ouput file

#fragment start core
	writeln("Here are the characters returned by the scanner:")
	writeln("  line col  character")

	# create a scanner (an instance of the Scanner class)
	scanner.initialize(sourceText)

	#------------------------------------------------------------------
	# Call the scanner's get() method repeatedly
	# to get the characters in the sourceText.
	# Stop when we reach the ENDMARK.
	#------------------------------------------------------------------
	character = scanner.get()       # getfirst Character object from the scanner
	while True:
		writeln(character)
		if character.cargo == scanner.ENDMARK: break
		character = scanner.get()   # getnext
#fragment stop core

	f.close()  # close the output file


#-----------------------------------------
#              run
#-----------------------------------------
if __name__ == "__main__":
	outputFilename = "output\\genericScannerDriver_output.txt"
	sourceText = open("input\\nxx1.txt").read()
	main(sourceText)
	print open(outputFilename).read()
