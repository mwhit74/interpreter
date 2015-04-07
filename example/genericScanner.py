#fragment start *
#fragment start 1
from genericCharacter import *

"""
A Scanner object reads through the sourceText
and returns one character at a time.
"""
#-------------------------------------------------------------------
#
#-------------------------------------------------------------------
def initialize(sourceTextArg):
	global sourceText, lastIndex, sourceIndex, lineIndex, colIndex
	sourceText = sourceTextArg
	lastIndex    = len(sourceText) - 1
	sourceIndex  = -1
	lineIndex    =  0
	colIndex     = -1


#-------------------------------------------------------------------
#
#-------------------------------------------------------------------
def get():
	"""
	Return the next character in sourceText.
	"""
	global lastIndex, sourceIndex, lineIndex, colIndex

	sourceIndex += 1    # increment the index in sourceText

	# maintain the line count
	if sourceIndex > 0:
		if sourceText[sourceIndex - 1] == "\n":
			#-------------------------------------------------------
			# The previous character in sourceText was a newline
			# character.  So... we're starting a new line.
			# Increment lineIndex and reset colIndex.
			#-------------------------------------------------------
			lineIndex +=1
			colIndex  = -1

	colIndex += 1

	if sourceIndex > lastIndex:
		# We've read past the end of sourceText.
		# Return the ENDMARK character.
		char = Character(ENDMARK, lineIndex, colIndex, sourceIndex,sourceText)
	else:
		c    = sourceText[sourceIndex]
		char = Character(c, lineIndex, colIndex, sourceIndex, sourceText)

	return char
#fragment stop  1


#fragment start 2
#-------------------------------------------------------------------
#
#-------------------------------------------------------------------
def lookahead(offset=1):
	"""
	Return a string (not a Character object) containing the character
	at position:
			sourceIndex + offset
	Note that we do NOT move our current position in the sourceText.
	That is,  we do NOT change the value of sourceIndex.
	"""
	index = sourceIndex + offset

	if index > lastIndex:
		# We've read past the end of sourceText.
		# Return the ENDMARK character.
		return ENDMARK
	else:
		return sourceText[index]