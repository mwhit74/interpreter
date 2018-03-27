import pdb

INTEGER, PLUS, MINUS, EOF = "INTEGER", "PLUS", "MINUS", "EOF"

class Token(object):
    def __init__(self, value, type):
        self.value = value
        self.type = type

    def __str__(self):
        return "Token({}, {})".format(self.value, self.type)

    def __repr__(self):
        return self.__str__()



class Interpreter(object):
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos]
        self.current_token = None

    def error(self):
        raise Exception('Invalid syntax')
    ##Lexer##
    
    def integer(self):
        value = ""
        while self.current_char is not None and self.current_char.isdigit():
            value += self.current_char
            self.get_next_char()
        return int(value)
    
    def whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.get_next_char()
    
    def get_next_char(self):
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None
        else:
            self.current_char = self.text[self.pos]
    
    def get_next_token(self):
        while self.current_char is not None:
            if self.current_char.isspace():
                self.whitespace()
                continue
            elif self.current_char.isdigit():
                return Token(self.integer(), INTEGER)
            elif self.current_char == '+':
                self.get_next_char()
                return Token('+', PLUS)
            elif self.current_char == '-':
                self.get_next_char()
                return Token('-', MINUS)
    
            self.error()
    
        return Token(None, EOF)
    
    ##Parser/Interpreter##
    
    def check_type(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.get_next_token()
        else:
            self.error()
    
    def term(self):
        token = self.current_token
        self.check_type(INTEGER)
        return token.value
    
    def pi(self):
        self.current_token = self.get_next_token()
        result = self.term()
        while self.current_token.type is not EOF:
            if self.current_token.type == PLUS:
                self.check_type(PLUS)
                result = result + self.term()
            elif self.current_token.type == MINUS:
                self.check_type(MINUS)
                result = result - self.term()
        return result

def main():
    while True:
        try:
            text = raw_input("calc> ")
        except EOFError:
            break
        if not text:
            continue
        interp = Interpreter(text)
        pdb.set_trace()
        result = interp.pi()
        print result

if __name__ == "__main__":
    main()
