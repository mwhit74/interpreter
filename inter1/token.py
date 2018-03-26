
from scan import *

class LexerError(Exception): pass

#-------------------------------------------------
#
#                TOKEN
#
#-------------------------------------------------

class Token:
	"""
	A Token object is the kind of thing that the Lexer returns.
	It holds:
	- the text of the token (self.cargo)
	- the type of token (self.tokenType)
	- the line number and column index of where the 
	token starts (self.lineIndex, self.colIndex)
	"""
	
	def __init__(self, startChar):
		
		self.cargo = None
		self.sourceText = None
		self.lineIndex = None
		self.colIndex = None
		
		self.set_cargo(startChar.get_cargo())
		self.set_sourceText(startChar.get_sourceText())
		self.set_lineIndex(startChar.get_lineIndex())
		self.set_colIndex(startChar.get_colIndex())
		self.type = None
	
	def set_cargo(self, cargo):
		self.cargo = cargo
		
	def set_sourceText(self, sourceText):
		self.sourceText = sourceText
		
	def set_lineIndex(self, lineIndex):
		self.lineIndex = lineIndex
		
	def set_colIndex(self, colIndex):
		self.colIndex = colIndex
		
	def set_type(self, type):
		self.type = type
	
	def get_cargo(self):
		return self.cargo
		
	def get_sourceText(self):
		return self.sourceText
		
	def get_lineIndex(self):
		return self.lineIndex
	
	def get_colIndex(self):
		return self.colIndex
	
	def get_type(self):
		return self.type
		
	def show(self, showLineNumbers = False, **kwargs):
	
		align = kwargs.get("align", True)
		if align:
			tokenTypeLen = 12
			space = " "
		else:
			tokenTypeLen = 0
			space = ""
			
		if showLineNumbers:
			s = str(self.get_lineIndex()).rjust(6) + str(self.get_colIndex).rjust(4) + "   "
		else:
			s = ""
			
		if self.get_type() == self.get_cargo():
			s = s + "Symbol".ljust(tokenTypeLen, ".") + ":" + space + self.type
		elif self.get_type() == "Whitespace":
			s = s + "Whitespace".ljust(tokenTypeLen, ".") + ":" + space + repr(self.get_cargo())
		else:
			s = s + self.get_type().ljust(tokenTypeLen, ".") + ":" + space + self.get_cargo()
		return s
		
	