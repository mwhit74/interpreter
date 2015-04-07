
from char import *

class Scanner:

	def __init__(self, sourceTextArg):
		
		self.sourceText = ''
		self.lastIndex = -1
		self.sourceIndex = -1
		self.lineIndex = -1
		self.colIndex = -1
		
		self.set_sourceText(sourceTextArg)
		self.set_lastIndex()
		self.set_sourceIndex()
		self.set_lineIndex()
		self.set_colIndex()
		
	def set_sourceText(self, sourceTextArg):
		self.sourceText = sourceTextArg
		
	def set_lastIndex(self):
		self.lastIndex = len(self.get_sourceText()) - 1
		
	def set_sourceIndex(self, sourceIndex = -1):
		#start counting source at 0
		self.sourceIndex = sourceIndex
		
	def set_lineIndex(self, lineIndex = 0):
		self.lineIndex = lineIndex
		
	def set_colIndex(self, colIndex = -1):
		#start counting colIndex at 0
		self.colIndex = colIndex
		
	def get_sourceText(self):
		return self.sourceText
		
	def get_lastIndex(self):
		return self.lastIndex
		
	def get_sourceIndex(self):
		return self.sourceIndex
		
	def get_lineIndex(self):
		return self.lineIndex
		
	def get_colIndex(self):
		return self.colIndex 
		
	def next(self):
		"""
		Return the next character in the sourceText.
		"""

		self.set_sourceIndex(self.get_sourceIndex() + 1)
		
		#line count
		if self.get_sourceIndex() > 0:
			if self.get_sourceText()[self.get_sourceIndex() - 1] == "\n":
				#Previous character was a newline
				#increment lineIndex
				#reset colIndex
				self.set_lineIndex(self.get_lineIndex() + 1)
				self.set_colIndex(-1)
				
		self.set_colIndex(self.get_colIndex() + 1)
		
		if self.get_sourceIndex() > self.get_lastIndex():
			#read past end of file
			#return ENDMARK character
			char = Character(ENDMARK,
							self.get_sourceIndex(), 			
							self.get_lineIndex(), 
							self.get_colIndex(), 
							self.get_sourceText())
		else:
			c = self.get_sourceText()[self.get_sourceIndex()]
			char = Character(c,
							self.get_sourceIndex(), 			
							self.get_lineIndex(), 
							self.get_colIndex(), 
							self.get_sourceText())
							
		return char
	
	def peek(self, offset = 1):
		"""
		Peek at the next char.
		"""
		
		index = self.get_sourceIndex() + offset
		
		if index > self.get_lastIndex():
			#read past end of file
			#return ENDMARK character
			char = Character(ENDMARK, 
							self.get_sourceIndex(), 			
							self.get_lineIndex(), 
							self.get_colIndex(), 
							self.get_sourceText())
		else:
			c = self.get_sourceText()[index]
			char = Character(c,
							self.get_sourceIndex(), 			
							self.get_lineIndex(), 
							self.get_colIndex(), 
							self.get_sourceText())
		
		return char
							
	def prev(self):
		"""
		Return the previous character in the sourceText.
		"""
		
		prev_sourceIndex = self.get_sourceIndex() - 1
		
		#prev line count
		if self.get_sourceIndex > 0:
			if self.get_sourceText()[self.get_sourceIndex() + 1] == "\n":
				prev_lineIndex = self.get_lineIndex() - 1
				prev_colIndex = prev_sourceIndex / prev_lineIndex
			else:
				prev_lineIndex = self.get_lineIndex()
				prev_colIndex = self.get_colIndex() - 1
				
		if self.get_sourceIndex() == self.get_lastIndex():
			char = Character(ENDMARK, 
							self.get_sourceIndex(), 			
							self.get_lineIndex(), 
							self.get_colIndex(), 
							self.get_sourceText())
		else:
			c = self.get_sourceText()[prev_sourceIndex]
			char = Character(c, 
							self.get_sourceIndex(), 			
							self.get_lineIndex(), 
							self.get_colIndex(), 
							self.get_sourceText())
							
		return char