#fragment start *
Keywords = []

#----------------------------------------------------------
# a list of symbols that are one character long
#----------------------------------------------------------
OneCharacterSymbols = """
=
( )
< >
[ ]
{ }
|
.
""" 
OneCharacterSymbols = OneCharacterSymbols.split()

TwoCharacterSymbols = []
#fragment start constants
import string

IDENTIFIER_STARTCHARS = string.letters
IDENTIFIER_CHARS      = string.letters + string.digits + "_"

NUMBER_STARTCHARS     = ""
NUMBER_CHARS          = ""

STRING_STARTCHARS = "'" + '"'
WHITESPACE_CHARS  = " \t\n"
#fragment stop constants

#-----------------------------------------------------------------------
# TokenTypes for things other than symbols and keywords
#-----------------------------------------------------------------------
STRING             = "Terminal"
IDENTIFIER         = "Nonterminal"
NUMBER             = "Number"
WHITESPACE         = "Whitespace"
COMMENT            = "Comment"
EOF                = "Eof"
 