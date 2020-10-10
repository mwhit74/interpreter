# from scratch, let's see how this goes
# i always want to try a functional programming approach
# the object-oriented apporach just seems excessive
# why do you need to have objects? it's not like you are creating
# multiple instances of your interpreter, are you?

from collections import namedtuple
import string
import pdb
import sys

INTEGER, PLUS, MINUS, MULTIPLY, DIVIDE, CHAR, EOF = ['INTEGER', 'PLUS',
'MINUS', 'MULTIPLY', 'DIVIDE', 'CHAR', 'EOF'] 

WHITESPACE = string.whitespace

#named tuple, similar to a struct in C
Token = namedtuple('Token', ['token_type', 'token_value'])

pos = 0
cur_char = ''
text = ''
cur_token = None

def error():
    raise Exception('Parsing value error')

def get_next_char():
    global pos
    global cur_char
    pos += 1
    if pos > len(text) - 1:
        cur_char = None
    else:
        cur_char = text[pos]


def get_whitespace():
    global cur_char
    value = '' 
    while cur_char != None and cur_char in WHITESPACE:
        value = value + cur_char
        get_next_char()
    
    return value

def get_integer():
    global cur_char
    value = ''
    while cur_char != None and cur_char.isdigit():
        value = value + cur_char
        get_next_char()

    return int(value)

def get_char():
    global cur_char
    value = ''
    while cur_char != None and cur_char.isalpha():
        value = value + cur_char
        get_next_char()

    return value
    
def get_next_token(): 
    global cur_char

    while cur_char != None:

        if cur_char in WHITESPACE:
            value = get_whitespace()
            token = Token(WHITESPACE, value)

        if cur_char.isdigit():
            value = get_integer()
            token = Token(INTEGER, value)
            return token

        if cur_char == '+':
            value = cur_char
            token = Token(PLUS, value)
            get_next_char()
            return token

        if cur_char == '-':
            value = cur_char
            token = Token(MINUS, value)
            get_next_char()
            return token

        if cur_char == '*':
            value = cur_char
            token = Token(MULTIPLY, value)
            get_next_char()
            return token

        if cur_char == '/':
            value = cur_char
            token = Token(DIVIDE, value)
            get_next_char()
            return token

        if cur_char.isalpha():
            value = get_char()
            token = Token(CHAR, value)
            return token

        error()

    return Token(EOF, None)


def check_token_type(token, token_type):
    global cur_token
    #print(cur_token)
    if token.token_type == token_type:
        cur_token = get_next_token()
    else:
        error()

def term():
    global cur_token
    token = cur_token
    check_token_type(token, INTEGER)
    return token.token_value


def parse_and_interpret_expr():
    global cur_token
    global cur_char

    cur_char = text[pos]
    cur_token = get_next_token() #get the first token

    if cur_token.token_value == 'q':
        sys.exit()
    
    result = term()
    while cur_token.token_type in (PLUS, MINUS) and cur_token.token_type != EOF:
        if cur_token.token_type == PLUS:
            check_token_type(cur_token, PLUS)
            result = result + term()
        elif cur_token.token_type == MINUS:
            check_token_type(cur_token, MINUS)
            result = result - term()
        else:
            error()

    while cur_token.token_type in (MULTIPLY, DIVIDE) and cur_token.token_type != EOF:
        if cur_token.token_type == MULTIPLY:
            check_token_type(cur_token, MULTIPLY)
            result = result * term()
        elif cur_token.token_type == DIVIDE:
            check_token_type(cur_token, DIVIDE)
            result = result / term()
        else:
            error()

    return result

def parse_token():

    pass

def interpret_expr():

    parse_tokens()


if __name__ == "__main__":

    while True:
        cur_token = None
        pos = 0
        cur_char = ''
        text = ''
        try:
            text = input('calc>')
        except EOFError:
            break

        result = parse_and_interpret_expr()
        print(result)
