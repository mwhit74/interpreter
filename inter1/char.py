
ENDMARK = "\0"

"""
A Character object holds
	- on character (self.cargo)
	- the index of character's position in the sourceText
	- the index of the line where the character was found in the sourceText
	- the index of the column where the character was found in the sourceText
	- (a referece to) the entire sourceText (self.sourceText)
	
The information listed above will be available to a token that uses this character.
If an error occurs, the token can use this information to report the line/column number
were the error occurred, and to show an image of the line in the sourceText where the 
error occurred.
"""

class Character:
	def __init__(self, cargo, sourceIndex, lineIndex, colIndex, sourceText):
	
		self.cargo = ""
		self.sourceIndex = -1
		self.lineIndex = -1
		self.colIndex = -1
		self.sourceText = ""
		
		self.set_cargo(cargo)
		self.set_sourceIndex(sourceIndex)
		self.set_lineIndex(lineIndex)
		self.set_colIndex(colIndex)
		self.set_sourceText(sourceText)
		
	#------------------
	# Set Methods
	#------------------
	#A relic from my C++ training 
	#that I can't let go of
		
	def set_cargo(self, cargo):
		self.cargo = cargo
	
	def set_sourceIndex(self, sourceIndex):
		self.sourceIndex = int(sourceIndex)
		
	def set_lineIndex(self, lineIndex):
		self.lineIndex = int(lineIndex)
		
	def set_colIndex(self, colIndex):
		self.colIndex = int(colIndex)
		
	def set_sourceText(self, sourceText):
		self.sourceText = str(sourceText)
		
	#------------------
	# Get Methods
	#------------------
		
	def get_cargo(self):
		return self.cargo
		
	def get_sourceIndex(self):
		return self.sourceIndex
		
	def get_lineIndex(self):
		return self.lineIndex
	
	def get_colIndex(self):
		return self.colIndex
		
	def get_sourceText(self):
		return self.sourceText
		
	#--------------------------------------------------------------
	# A displayable string representation of the Character object
	#--------------------------------------------------------------
		
	def __str__(self):
		
		cargo = self.get_cargo()
		if   cargo == " "     : cargo = "   space"
		elif cargo == "\n"    : cargo = "   newline"
		elif cargo == "\t"    : cargo = "   tab"
		elif cargo == ENDMARK : cargo = "   eof"
		
		
		return (
				str(self.get_lineIndex()).rjust(6)
			+	str(self.get_colIndex()).rjust(4)
			+	"     "
			+	cargo
			)

#Provide verification			
	#def _verify
		
#if __name__ == "__main__":
#	if len(sys.argv) == 2 and sys.argv == "verify":
#		char = Character("a", 1, 0, 1, "a")
#		print char
#		print "Line Index = 0"
#		print "Column Index = 1"
#		print "Cargo = 'a'"
		