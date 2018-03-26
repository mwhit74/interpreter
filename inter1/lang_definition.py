
#------------------------
#List of keywords
#------------------------

#Defintion of keywords:
#Primary (root nodes? function?)
#iden - Identifier of project or run
#join - Joint indicies
#memb - Member incidences
#incl - Include another file
#geom - Member geometry

#Secondary (leaf nodes? data for function?)
#tabl - Member geometry specification; geom

Keywords = """
iden
join
memb
tabl
incl
geom
"""
Keywords = Keywords.split()

#----------------------------------
# a list of one character symbols
#----------------------------------
OneCharacterSymbols = """
=
$
"""
OneCharacterSympbols = OneCharacterSymbols.split()

#----------------------------------
# a list of two character symbols
#----------------------------------

#Don't have any for now
#Leaving as a reminder

import string

######
######
#How to only allow digits or letters and not mix the two
######
######

IDENTIFIER_STARTCHARS = string.letters + string.digits
IDENTIFIER_CHARS      = string.letters

NUMBER_STARTCHARS     = string.digits
NUMBER_CHARS          = string.digits + "."


STRING_STARTCHARS     = "'" + '"'
WHITESPACE_CHARS      = " "

#--------------------------------------------------------
# TokenTypes for things other than symbols and keywords
#--------------------------------------------------------
STRING                = "String"
IDENTIFIER            = "Identifier"
NUMBER                = "Number"
WHITESPACE            = "Whitespace"
COMMENT               = "Comment"
EOF                   = "Eof"