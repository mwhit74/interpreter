
from collections import namedtuple
import string
import math

ADD, SUB, MUL, DIV, EOF = 'ADD','SUB','MUL','DIV','EOF'
OPA, CPA, POW, CAR, NUM = 'OPA','CPA','POW','CAR','NUM'
WS = 'WS'

WHITESPACE = string.whitespace

Token = namedtuple('Token',['token_type','token_value'])

class Lexer(object):

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]
        self.cur_token = None

    def error(self):
        raise ValueError('Character not recognized.')

    def get_next_char(self):
        self.pos += 1
        if self.pos <= len(self.text) - 1:
            self.cur_char = self.text[self.pos]
        else:
            self.cur_char = None

    def get_whitespace(self):
        value = ''
        while (self.cur_char != None and
                self.cur_char in WHITESPACE):
            value = value + self.cur_char
            self.get_next_char()
        
    def get_num(self):
        value = ''
        while (self.cur_char != None and
                self.cur_char.isdigit()):
            value = value + self.cur_char
            self.get_next_char()

        return int(value)
            
    def get_char(self):
        value = ''
        while (self.cur_char != None and
                self.cur_char.isalpha()):
            value = value + self.cur_char
            self.get_next_char()
        
        return value

    def get_next_token(self):

        while self.cur_char != None:

            if self.cur_char in WHITESPACE:
                self.get_whitespace()
            if self.cur_char.isdigit():
                value = self.get_num()
                token = Token(NUM, value)
                return token
            if self.cur_char.isalpha():
                value = self.get_char()
                token = Token(CAR, value)
                return token
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
                token = Token(OPA, self.cur_char)
                self.get_next_char()
                return token
            if self.cur_char == ')':
                token = Token(CPA, self.cur_char)
                self.get_next_char()
                return token
            if self.cur_char == '^':
                token = Token(POW, self.cur_char)
                self.get_next_char()
                return token

            self.error()

        return Token(EOF, None)


class Interpreter(object):

    def __init__(self,lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise SyntaxError('Invalid syntax.')

    def check_token_type(self, token_type):
        #print(self.cur_token)
        if self.cur_token.token_type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error()

    def expr1(self):
        result = self.expr2()

        while (self.cur_token.token_type != EOF and
            self.cur_token.token_type in (ADD,SUB)):
            
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
        result = self.expr4()

        while (self.cur_token.token_type != EOF and
            self.cur_token.token_type == POW):

            self.check_token_type(POW)
            result = math.pow(result, self.expr4())

        return result

    def expr4(self):

        if self.cur_token.token_type == OPA:
            self.check_token_type(OPA)
            result = self.expr1()
            self.check_token_type(CPA)
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
        lexer = Lexer(text)
        interpreter = Interpreter(lexer)
        result = interpreter.expr1()
        print(result)

if __name__ == "__main__":
    main()
