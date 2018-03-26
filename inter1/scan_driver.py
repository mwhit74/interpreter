
from scan import *

print "Here are the characters returned by the scanner:"
print "    line col   char"

#creater a scanner 
sourceText = open("E:\Projects\Python\interpreter\sourceText.txt", 'r').read()
scanner = Scanner(sourceText)
char = scanner.next() #get first char of file
while True:
	print (char) #print formatted string from character class
	if char.cargo == ENDMARK: break #break when eof is reached
	char = scanner.next() # get next