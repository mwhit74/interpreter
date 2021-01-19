
from collections import namedtuple
import string
import pdb

ADD, SUB, MUL, DIV = ('ADD', 'SUB', 'MUL', 'DIV')
NUM, OPAR, CPAR, EOF = ('NUM', 'OPAR', 'CPAR', 'EOF')
BEGIN, END, SEMI, DOT = ('BEGIN', 'END', 'SEMI', 'DOT')
ID, ASSIGN = ('ID', 'ASSIGN')

Token = namedtuple('Token', ['type', 'value'])

RESERVED_KEYWORDS = {
    'BEGIN': Token(BEGIN, 'BEGIN'),
    'END': Token(END, 'END'),
}

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

    def peek_next_char(self):
        peek_pos = self.pos + 1
        if peek_pos <= len(self.text) - 1:
            return self.text[peek_pos]
        else:
            return None
            
    def get_whitespace(self):
        value = ''
        while self.cur_char != None and self.cur_char.isspace():
            value = value + self.cur_char
            self.get_next_char()

    def get_num(self):
        value = ''
        while self.cur_char != None and self.cur_char.isdigit():
            value = value + self.cur_char
            self.get_next_char()

        return int(value)
    
    def _id(self):
        value = ''
        while (self.cur_char != None and 
                (self.cur_char.isalnum() or self.cur_char == '_')):
            value = value + self.cur_char
            self.get_next_char()

        value = value.upper()

        token = RESERVED_KEYWORDS.get(value, Token(ID, value))
        return token

    def get_next_token(self):
        op_dict = {'+':Token(ADD,'+'),
                   '-':Token(SUB,'-'),
                   '*':Token(MUL,'*'),
                   '/':Token(DIV,'/'),
                   '(':Token(OPAR,'('),
                   ')':Token(CPAR,')'),
                   ';':Token(SEMI,';'),
                   '.':Token(DOT,'.')}

        while self.cur_char != None:
            if self.cur_char.isspace():
                self.get_whitespace()
                continue
            if self.cur_char.isdigit():
                value = self.get_num()
                return Token(NUM, value)
            if self.cur_char.isalpha() or self.cur_char == '_':
                token = self._id()
                return token
            if self.cur_char == ':' and self.peek_next_char() == '=':
                self.get_next_char()
                self.get_next_char()
                return Token(ASSIGN, ':=')
            if self.cur_char in list(op_dict):
                token = op_dict[self.cur_char]
                self.get_next_char()
                return token

            self.error()

        return Token(EOF, None)
    

class AST(object):
    pass

class BinOp(AST):
    
    def __init__(self, left, token, right):
        self.left = left
        self.token = self.op = token
        self.right = right

class UnaryOp(AST):
    
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr

class Num(AST):

    def __init__(self, token):
        self.token = token
        self.value = token.value

class Compound(AST):

    def __init__(self):
        self.children = []

class Assign(AST):

    def __init__(self, left, op, right):
        self.left = left
        self.token = op
        self.right = right

class Var(AST):

    def __init__(self, token):
        self.token = token
        self.value = token.value

class NoOp(AST):
    pass       

class Parser(object):

    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise SyntaxError("Incorrect syntax.")

    def check_token_type(self, token_type):
        if self.cur_token.type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error

    def program(self):
        """program : compound_statement DOT"""

        node = self.compound_statement()
        self.check_token_type(DOT)
        return node

    def compound_statement(self):
        """compound_statement : BEGIN statement_list END"""
        self.check_token_type(BEGIN)
        nodes = self.statement_list()
        self.check_token_type(END)

        root = Compound()
        for node in nodes:
            root.children.append(node)

        return root

    def statement_list(self):
        """
        statement_list : statement 
                        | statement SEMI statement_list
        """
        
        node = self.statement()

        results = [node]

        while self.cur_token.type == SEMI:
            self.check_token_type(SEMI)
            results.append(self.statement())

        if self.cur_token.type == ID:
            self.error()

        return results

    def statement(self):
        """
        statement : compound_statement
                    | assignment_statement
                    | empty
        """

        if self.cur_token.type == BEGIN:
            node = self.compound_statement()
        elif self.cur_token.type == ID:
            node = self.assignment_statement()
        else:
            node = self.empty_statement()

        return node

    def assignment_statement(self):
        """
        assignment_statement : variable ASSIGN expr
        """

        left = self.variable()

        assign_op = self.cur_token
        self.check_token_type(ASSIGN)
        
        right = self.expr1()

        node = Assign(left, assign_op, right)

        return node

    def variable(self):
        """variable : ID"""
        node = Var(self.cur_token)
        self.check_token_type(ID)
        return node

    def empty_statement(self):
        """Empty production"""
        return NoOp()

    def expr1(self):
        node = self.expr2()

        while self.cur_token.type in (ADD, SUB): 
            token = self.cur_token
            if token.type == ADD:
                self.check_token_type(ADD)    
            if token.type == SUB:
                self.check_token_type(SUB)

            node = BinOp(left=node, token=token, right=self.expr2())

        return node

    def expr2(self):
        node = self.expr3()
   
        while self.cur_token.type in (MUL, DIV): 
            token = self.cur_token
            if token.type == MUL:
                self.check_token_type(MUL) 
            if token.type == DIV:
                self.check_token_type(DIV)

            node = BinOp(left=node, token=token, right=self.expr3())

        return node

    def expr3(self):
        token = self.cur_token

        if token.type == ADD:
            self.check_token_type(ADD)
            return UnaryOp(token, self.expr3())
        if token.type == SUB:
            self.check_token_type(SUB)
            return UnaryOp(token, self.expr3())
        if token.type == NUM:
            self.check_token_type(NUM)
            return Num(token)
        if token.type == OPAR:
            self.check_token_type(OPAR)
            result = self.expr1()
            self.check_token_type(OPAR)
        else:
            node = self.variable()
            return node

    def parse(self):
        node = self.program()
        if self.cur_token.type != EOF:
            self.error()

        return node


class NodeVisitor(object):

    def visit(self, node):
        method_name = "visit_" + type(node).__name__
        visitor_function = getattr(self, method_name, self.default_visit)
        return visitor_function(node)

    def default_visit(self, node):
        raise Exception(f'Method visit_{type(node).__name__} not defined.')

class Interpreter(NodeVisitor):

    GLOBAL_SCOPE = {}

    def __init__(self, parser):
        self.parser = parser

    def visit_Num(self, node):
        return node.value

    def visit_UnaryOp(self, node):
        token_type = node.op.type

        if token_type == ADD:
            result = +self.visit(node.expr)
        if token_type == SUB:
            result = -self.visit(node.expr)

        return result

    def visit_BinOp(self, node):
        left = node.left
        token_type = node.token.type
        right = node.right
        
        if token_type == ADD:
            result = self.visit(left) + self.visit(right)
        if token_type == SUB:
            result = self.visit(left) - self.visit(right)
        if token_type == MUL:
            result = self.visit(left) * self.visit(right)
        if token_type == DIV:
            result = self.visit(left) / self.visit(right)

        return result

    def visit_Compound(self, node):

        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node):
        var_name = node.left.value
        self.GLOBAL_SCOPE[var_name] = self.visit(node.right)

    def visit_Var(self, node):
        var_name = node.value
        value = self.GLOBAL_SCOPE.get(var_name)
        if value is None:
            raise NameError(repr(var_name))
        else:
            return value

    def visit_NoOp(self, node):
        pass

    def interpret(self):
        tree = self.parser.parse()
        if tree is None:
            return ''
        return self.visit(tree)

def main():
    import sys
    text = open(sys.argv[1], 'r').read()

    #pdb.set_trace()
    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    result = interpreter.interpret()
    print(interpreter.GLOBAL_SCOPE)

if __name__ == "__main__":
    main()
