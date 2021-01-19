import pdb
import string

PLUS, MINUS, MULTIPLY, DIVIDE, EOF, NUM, CHAR = ('PLUS', 
    'MINUS', 'MULTIPLY', 'DIVIDE', 'EOF', 'NUM', 'CHAR')

WS = string.whitespace

class Token(object):
    
    def __init__(self, token_type, token_value):
        self.token_type = token_type
        self.token_value = token_value

    def __str__(self):
        return f'Token({self.token_type},{self.token_value})'

    def __repr__(self):
        return self.__str__()

class Lexer(object):

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]
        self.cur_token = None

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
        while self.cur_char != None and self.cur_char in WS:
            value = value + self.cur_char
            self.get_next_char()

        return value

    def get_chars(self):
        value = ''
        while self.cur_char != None and self.cur_char.isalpha():
            value = value + self.cur_char
            self.get_next_char()

        return value

    def get_ints(self):
        value = ''
        while self.cur_char != None and self.cur_char.isdigit():
            value = value + self.cur_char
            self.get_next_char()

        return int(value)


    def get_next_token(self):

        while self.cur_char != None:

            if self.cur_char in WS:
                value = self.get_whitespace()

            if self.cur_char.isalpha():
                value = self.get_chars()
                token = Token(CHAR, value)
                return token

            if self.cur_char.isdigit():
                value = self.get_ints()
                token = Token(NUM, value)
                return token

            if self.cur_char == '+':
                value = self.cur_char
                self.get_next_char()
                return Token(PLUS, value)

            if self.cur_char == '-':
                value = self.cur_char
                self.get_next_char()
                return Token(MINUS, value)

            if self.cur_char == '*':
                value = self.cur_char
                self.get_next_char()
                return Token(MULTIPLY, value)

            if self.cur_char == '/':
                value = self.cur_char
                self.get_next_char()
                return Token(DIVIDE, value)
            
            self.error()
            
        return Token(EOF, None)

class Interpreter(object):

    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise ValueError('Syntax error')
    
    def check_token_type(self, token_type):
        if self.cur_token.token_type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error()

    def factor(self):
        value = self.cur_token.token_value
        self.check_token_type(NUM)
        return value

    def expr(self):

        result = self.factor()

        while (self.cur_token.token_type != EOF and 
            self.cur_token.token_type in (PLUS, MINUS, MULTIPLY, DIVIDE)):
            
            if self.cur_token.token_type == PLUS:
                self.check_token_type(PLUS)
                result = result + self.factor()
            if self.cur_token.token_type == MINUS:
                self.check_token_type(MINUS)
                result = result - self.factor()
            if self.cur_token.token_type == MULTIPLY:
                self.check_token_type(MULTIPLY)
                result = result * self.factor()
            if self.cur_token.token_type == DIVIDE:
                self.check_token_type(DIVIDE)
                result = result / self.factor()

        return result


def main():
    while True:
        try:
            text = input('calc>')
        except EOFError:
            break
        if not text:
            continue
        #pdb.set_trace()
        lexer = Lexer(text)
        interpreter = Interpreter(lexer)
        result = interpreter.expr()
        print(result)


if __name__ == "__main__":
    main() 
