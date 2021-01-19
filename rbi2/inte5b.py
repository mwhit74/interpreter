import string
import pdb
from collections import namedtuple

ADD, SUB, MUL, DIV, CHAR = 'ADD','SUB','MUL','DIV','CHAR'
NUM, EOF, OPAR, CPAR = 'NUM','EOF','OPAR','CPAR'

WHITESPACE = string.whitespace

Token = namedtuple('Token',['token_type', 'token_value'])

class Lexer(object):
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]

    def error(self):
        raise ValueError('Invalid character')
    
    def get_next_char(self):
        self.pos += 1
        if self.pos <= len(self.text) - 1:
            self.cur_char = self.text[self.pos]
        else:
            self.cur_char = None

    def get_whitespace(self):
        value = ''
        while self.cur_char != None and self.cur_char in WHITESPACE:
            value = value + self.cur_char
            self.get_next_char()

    def get_num(self):
        value = ''
        while self.cur_char != None and self.cur_char.isdigit():
            value = value + self.cur_char
            self.get_next_char()

        return int(value)

    def get_chars(self):
        value = ''
        while self.cur_char != None and self.cur_char.isalpha():
            value = value + self.cur_char
            self.get_next_char()

        return value

    def get_next_token(self):

        while self.cur_char != None:

            if self.cur_char in WHITESPACE:
                value = self.get_whitespace()

            if self.cur_char.isdigit():
                value = self.get_num()
                return Token(NUM, value)
            
            if self.cur_char.isalpha():
                value = self.get_chars()
                return Token(CHAR, value)

            if self.cur_char == '+':
                token = Token(ADD, self.cur_char)
                self.get_next_char()
                return token
            
            if self.cur_char == '-':
                token = Token(SUB, self.cur_char)
                self.get_next_char()
                return token

            if self.cur_char == '*':
                token = Token(MUL, self.cur_char)
                self.get_next_char()
                return token

            if self.cur_char == '/':
                token = Token(DIV, self.cur_char)
                self.get_next_char()
                return token
            
            if self.cur_char == '(':
                token = Token(OPAR, self.cur_char)
                self.get_next_char()
                return token

            if self.cur_char == ')':
                token = Token(CPAR, self.cur_char)
                self.get_next_char()
                return token

            self.error()

        return Token(EOF, None)


class Interpreter(object):
    
    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise SyntaxError('Invalid syntax')

    def check_token_type(self, token_type):
        #print(self.cur_token)
        #pdb.set_trace()
        if self.cur_token.token_type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error() 

    def expr1(self):
        result = self.expr2()

        while (self.cur_token.token_type != EOF and
                 self.cur_token.token_type in (ADD, SUB)):
            
            if self.cur_token.token_type == ADD:
                self.check_token_type(ADD)
                result = result + self.expr2()
            if self.cur_token.token_type == SUB:
                self.check_token_type(SUB)
                result = result - self.expr2()

        return result

    def expr2(self):
        result = self.expr3()

        while (self.cur_token.token_type != EOF and
                self.cur_token.token_type in (MUL, DIV)):

            if self.cur_token.token_type == MUL:
                self.check_token_type(MUL)
                result = result * self.expr3()
            if self.cur_token.token_type == DIV:
                self.check_token_type(DIV)
                result = result / self.expr3()

        return result

    def expr3(self):
        if self.cur_token.token_type == OPAR:
            self.check_token_type(OPAR)
            result = self.expr1()
            self.check_token_type(CPAR)
        else:
            result = self.factor()

        return result

    def factor(self):
        value = self.cur_token.token_value
        self.check_token_type(NUM)
        return value

def main():
    while True:
        try:
            text = input('calc>')
        except e:
            continue
        #pdb.set_trace()
        lexer = Lexer(text)
        interpreter = Interpreter(lexer)
        result = interpreter.expr1()
        print(result)

if __name__ == "__main__":
    main()
