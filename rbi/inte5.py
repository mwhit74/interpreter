INTEGER, PLUS, MINUS, MUL, DIV, OPAR, CPAR, EOF = ("INTEGER", "PLUS", "MINUS", "MUL", "DIV", "OPAR",
"CPAR", "EOF")

class Token(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        return "Token({0}, {1})".format(type,repr(value))

    def __repr__(self):
        return self.__str__()


class Lexer(object):
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]

    def advance(self):
        self.pos += 1
        self.cur_char = self.text[self.pos]

    def error(self):
        raise Exception("Invalid syntax")

    def whitespace(self):
        while self.cur_char is not None and self.cur_char.isspace():
            self.advance()

    def integer(self):
        val = 0
        while self.cur_char is not None and self.cur_char.isdigit():
            val += int(self.cur_char)
            self.advance()
        return val

    def get_next_token(self):
        while self.cur_char is not None:
            if self.cur_char.isspace():
                self.whitespace()
                continue
            elif self.cur_char.isdigit():
                return Token(INTEGER, self.integer())
            elif self.cur_char == "+":
                self.advance()
                return Token(PLUS, "+")
            elif self.cur_char == "-":
                self.advance()
                return Token(MINUS, "-")
            elif self.cur_char == "*":
                self.advance()
                return Token(MUL, "*")
            elif self.cur_char == "/":
                self.advance()
                return Token(DIV, "/")
            elif self.cur_char == "(":
                self.advance()
                return Token(OPAR, "(")
            elif self.cur_char == ")":
                self.advance()
                return Token(CPAR, ")")

            self.error()

        return Token(EOF, None)
                

class Interpreter(object):
    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = lexer.get_next_token()

    def error(self):
        raise Exception("Invalid syntax")

    def check_type(self, token_type):
        if self.cur_token.type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error()

    def factor(self):
        if self.cur_token.type == INTEGER:
            self.check_type(INTEGER)
            result = self.cur_token.value
        elif self.cur_token.type == OPAR:
            self.check_type(OPAR)
            result = self.expr()
            self.check_type(CPAR)

        return result

    def term(self):
        result = self.factor()
        while self.cur_token in (MUL, DIV):
            if self.cur_token.type == MUL:
                self.check_type(MUL)
                result = result * self.factor()
            elif self.cur_token.type == DIV:
                self.check_type(DIV)
                result = result / self.factor()

        return result
 
    def expr(self):
        result = self.term()
        while self.cur_token in (PLUS, MINUS):
            if self.cur_token.type == PLUS:
                self.check_type(PLUS)
                result += self.term()
            elif self.cur_token.type == MINUS:
                self.check_type(MINUS)
                result -= self.term()

        return result

def main():
    while True:
        try:
            text = raw_input("calc>")
        except EOFerror:
            break
        if not text:
            continue
        else:
            lexer = Lexer(text)
            inte = Interpreter(lexer)
            print(inte.expr())

if __name__ == "__main__":
    main()
