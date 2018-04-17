import pdb

INTEGER = "INTEGER"
PLUS = "PLUS"
MINUS = "MINUS"
MUL = "MUL"
DIV = "DIV"
OPAR = "OPAR"
CPAR = "CPAR"
EOF = "EOF"

class Token(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        return "Token({}, {})".format(self.type, self.value)

    def __repr__(self):
        return self.__str__()

class Lexer(object):
    """Returns a stream of tokens"""
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]

    def whitespace(self):
        while self.cur_char is not None and self.cur_char.isspace():
            self.get_next_char()

    def integer(self):
        value = ""
        while self.cur_char is not None and self.cur_char.isdigit():
            value += self.cur_char
            self.get_next_char()
        return int(value)

    def get_next_char(self):
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.cur_char = None
        else:
            self.cur_char = self.text[self.pos]

    def get_next_token(self):
        while self.cur_char is not None:
            if self.cur_char.isspace():
                self.whitespace()
                continue
            elif self.cur_char.isdigit():
                return Token(INTEGER, self.integer())
            elif self.cur_char == "+":
                self.get_next_char()
                return Token(PLUS, "+")
            elif self.cur_char == "-":
                self.get_next_char()
                return Token(MINUS, "-")
            elif self.cur_char == "*":
                self.get_next_char()
                return Token(MUL, "*")
            elif self.cur_char == "/":
                self.get_next_char()
                return Token(DIV, "/")
            elif self.cur_char == "(":
                self.get_next_char()
                return Token(OPAR, "(")
            elif self.cur_char == ")":
                self.get_next_char()
                return Token(CPAR, ")")

            self.error()

        return Token(EOF, None)

class Interpreter(object):
    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise Exception("Invalid syntax.")

    def factor(self):
        token = self.cur_token
        self.check_type(INTEGER)
        return token.value

    def check_type(self, token_type):
        if self.cur_token.type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error()
"""
    expr1: expr2((PLUS|MINUS)expr2)*
    expr2: expr3((MUL|DIV)expr3))*
    expr3: (OPAR(expr1)CPAR)*|factor
    factor: INTEGER
"""
    def expr1(self):
        result = self.expr2()
        while self.cur_token.type in (PLUS, MINUS):
            if self.cur_token.type is PLUS:
                self.check_type(PLUS)
                result += self.expr2()
            elif self.cur_token.type is MINUS:
                self.check_type(MINUS)
                result -= self.expr2()

        return result

    def expr2(self):
        result = self.expr3()
        while self.cur_token.type in (MUL, DIV):
            if self.cur_token.type is MUL:
                self.check_type(MUL)
                result = result * self.expr3()
            elif self.cur_token.type is DIV:
                self.check_type(DIV)
                result = result / self.expr3()

        return result

    def expr3(self):
        if self.cur_token.type is INTEGER:
            result = self.factor()
        elif self.cur_token.type is OPAR:
            self.check_type(OPAR)
            while self.cur_token.type is not CPAR:
                result = self.expr1()

            self.check_type(CPAR)
        return result

def main():
    #while True:
    #    try:
    #        text = raw_input("calc> ")
    #    except EOFError:
    #        break
    #    if not text:
    #        continue

    lexer = Lexer("7 + 3 * (10/(12/(3+1)-1))")
    #pdb.set_trace()
    inte = Interpreter(lexer)
    #pdb.set_trace()
    result = inte.expr1()
    print result

if __name__ == "__main__":
    main()
