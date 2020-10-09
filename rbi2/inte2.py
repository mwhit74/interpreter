import pdb
import string

#Token Types

INTEGER, PLUS, MINUS, MULTIPLY, DIVIDE, CHAR, WS, EOF = ['INTEGER', 'PLUS', 
'MINUS', 'MULTIPLY', 'DIVIDE', 'CHAR', 'WS', 'EOF']
WHITESPACE = string.whitespace

class Token(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        """String representation of class

        Examples:
            Token(INTEGER, 3)
            Token(PLUS, '+')
        """
        return f'Token({self.type}, {self.value})'

    def __repr__(self):
        return self.__str__()

class Interpreter(object):
    def __init__(self, text):
        #user string input
        self.text = text
        #self.pos is an index into self.text
        self.pos = 0
        #current token instance
        self.curent_token = None
        #this line is gold, it simplifies the code so much
        #ahh, I just can't get over the elegance of this line
        self.cur_char = self.text[self.pos]

    def error(self):
        raise Exception('Error parsing input')

    def get_next_char(self):
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.cur_char = None
        else:
            self.cur_char = self.text[self.pos]


    def get_int(self):
        value = ''
        while self.cur_char != None and self.cur_char.isdigit():
            value = value + self.cur_char
            self.get_next_char()

        return int(value)

    def get_char(self):
        value = ''
        while self.cur_char != None and self.cur_char.isalpha():
            value = value + self.cur_char
            self.get_next_char()

        return value

    def get_whitespace(self):
        value = ''
        while self.cur_char != None and self.cur_char in WHITESPACE:
            value = value + self.cur_char
            self.get_next_char()

        return value


    def get_next_token(self):
        """Lexical analyzer (also known as a scanner or tokenizer)

        This method is responsible for breaking a sentence apart
        into tokens. One token at a time.
        """

        while self.cur_char is not None: 
            if self.cur_char in WHITESPACE:
                value = self.get_whitespace()
                token = Token(WS, value)

            if self.cur_char.isdigit():
                value = self.get_int()
                token = Token(INTEGER, value) 
                return token

            if self.cur_char == '+' :
                token = Token(PLUS, self.cur_char)
                self.get_next_char()
                return token

            if self.cur_char == '-':
                token = Token(MINUS, self.cur_char)
                self.get_next_char()
                return token

            if self.cur_char.isalpha():
                value = self.get_char()
                token = Token(CHAR, value)
                return token

            self.error()

        return Token(EOF, None)

    def check_token_type(self, token_type):
        if self.cur_token.type == token_type:
            self.cur_token = self.get_next_token()
        else:
            self.error()

    def expr(self):
        """expr -> INTEGER PLUS INTEGER"""
        #set cur token to the first token taken from the input
        self.cur_token = self.get_next_token()

        if self.cur_token.value == 'q':
            self.check_token_type(CHAR)
            quit()

        else:
            #expect first token to be single digit int
            left = self.cur_token
            self.check_token_type(INTEGER)

            #expect second token to be '+' operator
            op = self.cur_token
            if op.type == PLUS:
                self.check_token_type(PLUS)
            else:
                self.check_token_type(MINUS)

            #expect third token to be single digit int
            right = self.cur_token
            self.check_token_type(INTEGER)

            #at this point INTEGER PLUS INTEGER token sequence
            #has been successfully found and the method can 
            #return the result of adding two integer, thus
            #effectively interpreting client input
            if op.type == PLUS:
                result = left.value + right.value
            else:
                result = left.value - right.value

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
        interpreter = Interpreter(text)
        result = interpreter.expr()
        print(result)

if __name__ == '__main__':
    main()

