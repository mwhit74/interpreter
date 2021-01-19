
from collections import namedtuple
from string import whitespace
import pdb

ADD, SUB, MUL, DIV = ('ADD', 'SUB', 'MUL', 'DIV')
NUM, OPAR, CPAR, EOF = ('NUM', 'OPAR', 'CPAR', 'EOF')
WS = whitespace

Token = namedtuple('Token', ['token_type', 'token_value'])

class Lexer(object):

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]

    def error(self):
        raise ValueError('Character not recoginized.')

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

    def get_num(self):
        value = ''
        while self.cur_char != None and self.cur_char.isdigit():
            value = value + self.cur_char
            self.get_next_char()

        return int(value)

    def get_next_token(self):
        
        while self.cur_char != None:
            if self.cur_char in WS:
                value = self.get_whitespace()
                token = Token(WS, value)
            if self.cur_char.isdigit():
                value = self.get_num()
                return Token(NUM, value)
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
        self.value = token.token_value

class Parser(object):

    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise SyntaxError('Invalid syntax.')

    def check_token_type(self, token_type):
        #pdb.set_trace()
        if self.cur_token.token_type == token_type:
            print(self.cur_token)
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error()

    def expr1(self):
        node = self.expr2()

        while self.cur_token.token_type in (ADD, SUB):
            token = self.cur_token
            if token.token_type == ADD:
                self.check_token_type(ADD)
            elif token.token_type == SUB:
                self.check_token_type(SUB)

            node = BinOp(left=node, op=token, right=self.expr2())

        return node

    def expr2(self):
        node = self.expr3()

        while self.cur_token.token_type in (MUL, DIV):
            token = self.cur_token
            if token.token_type == MUL:
                self.check_token_type(MUL)
            elif token.token_type == DIV:
                self.check_token_type(DIV)

            node = BinOp(left=node, op=token, right=self.expr3())

        return node

    def expr3(self):
        token = self.cur_token

        if token.token_type == NUM:
            self.check_token_type(NUM)
            node = Num(token)
        elif token.token_type == OPAR:
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
        raise Exception('Method visit_{} not defined.'.format(type(node).__name__))

class Interpreter(NodeVisitor):

    def __init__(self, parser):
        self.parser = parser

    #returns a post-fix notation string
    def visit_BinOp(self, node):
        if node.op.token_type == ADD:
            return f'{self.visit(node.left)} {self.visit(node.right)} +'
            #return self.visit(node.left) + self.visit(node.right)
        elif node.op.token_type == SUB:
            return f'{self.visit(node.left)} {self.visit(node.right)} -'
            #return self.visit(node.left) - self.visit(node.right)
        elif node.op.token_type == MUL:
            return f'{self.visit(node.left)} {self.visit(node.right)} *'
            #return self.visit(node.left) * self.visit(node.right)
        elif node.op.token_type == DIV:
            return f'{self.visit(node.left)} {self.visit(node.right)} /'
            #return self.visit(node.left) / self.visit(node.right)

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

def print_tokens(lexer):

    cur_token = lexer.get_next_token()
    while cur_token.token_type != EOF:
        print(cur_token)
        cur_token = lexer.get_next_token()
    print(cur_token)

if __name__ == "__main__":
    main()
