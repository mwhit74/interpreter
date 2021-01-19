
from collections import namedtuple
from string import whitespace
import pdb

ADD, SUB, MUL, DIV = ('ADD', 'SUB', 'MUL', 'DIV')
NUM, OPAR, CPAR, EOF = ('NUM', 'OPAR', 'CPAR', 'EOF')
WS = whitespace

Token = namedtuple('Token', ['type', 'value'])

class Lexer(object):

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]

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
        while self.cur_char != None and self.cur_char in WS:
            value = value + self.cur_char
            self.get_next_char()


    def get_num(self):
        value = ''
        while self.cur_char != None and self.cur_char.isdigit():
            value = value + self.cur_char
            self.get_next_char()

        return int(value)

    def get_next_token(self):
        op_dict = {'+':Token(ADD, self.cur_char),
                   '-':Token(SUB, self.cur_char),
                   '*':Token(MUL, self.cur_char),
                   '/':Token(DIV, self.cur_char),
                   '(':Token(OPAR, self.cur_char),
                   ')':Token(CPAR, self.cur_char)}

        while self.cur_char != None:
            if self.cur_char in WS:
                self.get_whitespace()
            if self.cur_char.isdigit():
                value = self.get_num()
                return Token(NUM, value)
            if self.cur_char in ('+','-','*','/','(',')'):
                token = op_dict[self.cur_char]
                self.get_next_char()
                return token

            self.error()

        return Token(EOF, None)


class AST(object):
    pass

class BinOp(AST):

    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class Num(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value

class Parser(object):

    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise SyntaxError('Syntax error')

    def check_token_type(self, token_type):
        #print(self.cur_token)
        if self.cur_token.type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error()

    def expr1(self):
        node = self.expr2()

        while self.cur_token.type in (ADD, SUB):
            token = self.cur_token
            if token.type == ADD:
                self.check_token_type(ADD)
            if token.type == SUB:
                self.check_token_type(SUB)

            node = BinOp(left=node, op=token, right=self.expr2())

        return node

    def expr2(self):
        node = self.expr3()

        while self.cur_token.type in (MUL, DIV):
            token = self.cur_token
            if token.type == MUL:
                self.check_token_type(MUL)
            if token.type == DIV:
                self.check_token_type(DIV)

            node = BinOp(left=node, op=token, right=self.expr3())

        return node

    def expr3(self):
        token = self.cur_token
        if token.type == NUM:
            self.check_token_type(NUM)
            node = Num(token)
        elif token.type == OPAR:
            self.check_token_type(OPAR)
            node = self.expr1()
            self.check_token_type(CPAR)

        return node


    def parse(self):
        return self.expr1()


class NodeVisitor(object):

    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception(f'Method visit_{type(node).__name__} not defined.')

class Interpreter(NodeVisitor):

    def __init__(self, parser):
        self.parser = parser

    def visit_BinOp(self, node):
        if node.op.type == ADD:
            return f'(+ {self.visit(node.left)} {self.visit(node.right)})'
        if node.op.type == SUB:
            return f'(- {self.visit(node.left)} {self.visit(node.right)})'
        if node.op.type == MUL:
            return f'(* {self.visit(node.left)} {self.visit(node.right)})'
        if node.op.type == DIV:
            return f'(/ {self.visit(node.left)} {self.visit(node.right)})'

    def visit_Num(self, node):
        return node.value

    def interpret(self):
        tree = self.parser.parse()
        return self.visit(tree)


def main():
    while True:
        try:
            text = input('spi>')
        except EOFError:
            break
        if not text:
            continue

        lexer = Lexer(text)
        parser = Parser(lexer)
        interpreter = Interpreter(parser)
        result = interpreter.interpret()
        print(result)

if __name__ == "__main__":
    main()
