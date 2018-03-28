import pdb
INTEGER, MUL, DIV, EOF = "INTEGER", "MUL", "DIV", "EOF"

class Token(object):
    def __init__(self, value, type):
        self.value = value
        self.type = type

    def __str__(self):
        return "Token({}, {})".format(self.value, self.type)

    def __repr__(self):
        return self.__str__()

class Lexer(object):
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos]

    def error(self):
        raise Exception("Invalid syntax.")

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
            if self.current_char.isdigit():
                return Token(self.integer(), INTEGER)
            if self.current_char == "*":
                self.get_next_char()
                return Token("*", MUL)
            elif self.current_char == "/":
                self.get_next_char()
                return Token("/", DIV)

            self.error()

        return Token(None, EOF)

class Interpreter(object):
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def error(self):
        raise Exception("Invalid syntax")

    def check_type(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def factor(self):
        token = self.current_token
        self.check_type(INTEGER)
        return token.value

    def pi(self):
        result = self.factor()
        while self.current_token.type is not EOF:
            if self.current_token.type is MUL:
                self.check_type(MUL)
                result = result * self.factor()
            elif self.current_token.type is DIV:
                self.check_type(DIV)
                result = result / self.factor()

        return result


def main():
    while True:
        try:
            text = raw_input("calc> ")
        except EOFError:
            break
        if not text:
            continue
        pdb.set_trace()
        lexer = Lexer(text)
        pdb.set_trace()
        interp = Interpreter(lexer)
        result = interp.pi()
        print result

if __name__ == "__main__":
    main()
