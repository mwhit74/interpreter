
from collections import namedtuple
from string import whitespace
import pdb
import sys

ADD         = 'ADD'
SUB         = 'SUB'
MUL         = 'MUL'
INT_DIV     = 'INT_DIV'
REAL_DIV    = 'REAL_DIV'
DOT         = 'DOT'
COLON       = 'COLON'
SEMI        = 'SEMI'
ASSIGN      = 'ASSIGN'
BEGIN       = 'BEGIN'
END         = 'END'
INT_CONST   = 'INT_CONST'
REAL_CONST  = 'REAL_CONST'
VAR         = 'VAR'
ID          = 'ID'
OPAR        = 'OPAR'
CPAR        = 'CPAR'
INT_TYPE    = 'INT_TYPE'
REAL_TYPE   = 'REAL_TYPE'
COMMA       = 'COMMA'
PROGRAM     = 'PROGRAM'
EOF         = 'EOF'

Token = namedtuple('Token', ['type', 'value'])

#lang. repr : Token(interpreter repr, lang. repr)
RESERVED_KEYWORDS = {
    'BEGIN'     : Token(BEGIN, 'BEGIN'),
    'END'       : Token(END, 'END'),
    'PROGRAM'   : Token(PROGRAM, 'PROGRAM'),
    'INTEGER'   : Token(INT_TYPE, 'INTEGER'),
    'REAL'      : Token(REAL_TYPE, 'REAL'),
    'VAR'       : Token(VAR, 'VAR'),
    'DIV'       : Token(INT_DIV, 'DIV'),
    }

class Lexer(object):

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]

    def error(self):
        raise ValueError('Character ' + self.cur_char + ' not recognized.')

    def get_next_char(self):
        #print(self.cur_char)
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
        while self.cur_char != None and self.cur_char in whitespace:
            value = value + self.cur_char
            self.get_next_char()

    def get_comment(self):
        value = ''
        while self.cur_char != None and self.cur_char != '}':
            value = value + self.cur_char
            self.get_next_char()

        #gets closing comment bracket, '}'
        value = value + self.cur_char
        self.get_next_char()

    def get_number(self):
        value = ''
        while self.cur_char != None and self.cur_char.isdigit():
            value = value + self.cur_char
            self.get_next_char()

            if self.cur_char == '.':
                value = value + self.cur_char 
                self.get_next_char()
                while self.cur_char.isdigit():
                    value = value + self.cur_char
                    self.get_next_char()

                return Token(REAL_CONST, float(value))

        return Token(INT_CONST, int(value))

    def get_id(self):
        value = ''
        while (self.cur_char != None and (
            self.cur_char.isalnum() or self.cur_char == '_')):
            value = value + self.cur_char
            self.get_next_char()

        #keywords and variables are not case sensitive
        value = value.upper()

        return RESERVED_KEYWORDS.get(value, Token(ID, value))

    def get_next_token(self):
        symbol_dict = {
            '+':Token(ADD, '+'),
            '-':Token(SUB, '-'),
            '*':Token(MUL, '*'),
            '/':Token(REAL_DIV, '/'),
            '.':Token(DOT, '.'),
            ';':Token(SEMI, ';'),
            ':':Token(COLON, ':'),
            ',':Token(COMMA, ','),
            '(':Token(OPAR, '('),
            ')':Token(CPAR, ')')
        }

        while self.cur_char != None:

            if self.cur_char in whitespace:
                self.get_whitespace()
                #very important, skips all additional statements and
                #starts the loop over again from the top
                continue 
            if self.cur_char == '{':
                self.get_comment()
                #very important, skips all additional statements and
                #starts the loop over again from the top
                continue
            if self.cur_char.isdigit():
                return self.get_number()
            if self.cur_char.isalpha() or self.cur_char == '_':
                return self.get_id()
            if self.cur_char == ':' and self.peek_next_char() == '=':
                self.get_next_char() #moves text pos to equals sign
                self.get_next_char() #moves text pos to next char after equals sign
                return Token(ASSIGN, ':=')
            if self.cur_char in list(symbol_dict):
                token = symbol_dict[self.cur_char]
                self.get_next_char()
                return token 
               
            self.error()

        return Token(EOF, None)


class AST(object):
    pass

class Program(AST):

    def __init__(self, name, block):
        self.name = name
        self.block = block

class Block(AST):

    def __init__(self, declarations, compound_statements):
        self.declarations = declarations
        self.compound_statements = compound_statements

class VarDecl(AST):

    def __init__(self, type_node, var_node):
        self.type_node = type_node
        self.var_node = var_node

class TypeSpec(AST):

    def __init__(self, token):
        self.token = token
        self.value = token.value

class Compound(AST):

    def __init__(self):
        self.statements = [] 

class Assign(AST):

    def __init__(self, left, token, right):
        self.left = left
        self.token = token
        self.right = right

class Empty(AST):
    pass

class BinOp(AST):

    def __init__(self, left, token, right):
        self.left = left
        self.token = self.op = token
        self.right = right

class UnaryOp(AST):

    def __init__(self, left, factor):
        self.left = self.op = left
        self.factor = factor

class Variable(AST):

    def __init__(self, token):
        self.token = token
        self.value = token.value

class Num(AST):

    def __init__(self, token):
        self.token = token
        self.value = token.value

class Parser(object):

    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise TypeError('Invalid token type.')

    def check_token_type(self, token_type):
        #print(self.cur_token)
        if self.cur_token.type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error()

    def program(self):
        self.check_token_type(PROGRAM)
        name = self.cur_token.value
        self.check_token_type(ID)
        self.check_token_type(SEMI)

        block = self.block()

        self.check_token_type(DOT)

        return Program(name, block)

    def block(self):
        declarations = self.declaration()
        compound_statements = self.compound_statement()

        return Block(declarations, compound_statements)

    def declaration(self):
    
        declarations = []
        
        if self.cur_token.value == 'VAR':
            self.check_token_type(VAR)
            while self.cur_token.type == ID:
                declarations.extend(self.vardec())
                self.check_token_type(SEMI)

        return declarations

    def vardec(self):

        var_nodes = [self.variable()]

        while self.cur_token.type == COMMA:
            self.check_token_type(COMMA)
            var_nodes.append(self.variable())

        if self.cur_token.type == ID:
            self.error()

        self.check_token_type(COLON)

        type_node = self.type_spec()

        var_decls = []
        for var_node in var_nodes:
            var_decls.append(VarDecl(type_node, var_node))

        return var_decls

    def type_spec(self):
        type_node = TypeSpec(self.cur_token)

        if self.cur_token.type == INT_TYPE:
            self.check_token_type(INT_TYPE)
        if self.cur_token.type == REAL_TYPE:
            self.check_token_type(REAL_TYPE)
        
        return type_node

    def compound_statement(self):
        self.check_token_type(BEGIN)
        statements = self.statement_list()
        self.check_token_type(END)

        compound = Compound()
        for statement in statements:
            compound.statements.append(statement)

        return compound

    def statement_list(self):

        statements = [self.statement()]
        
        while self.cur_token.type == SEMI:
            self.check_token_type(SEMI)
            statements.append(self.statement())

        return statements

    def statement(self):

        if self.cur_token.type == BEGIN:
            return self.compound_statement()
        if self.cur_token.type == ID:
            return self.assign_statement()
        else:
            """for the Empty statement, the prior SEMI would be checked
            which would advance to the next token which could be the
            last statement of a compound statement or an Empty
            statement followed by an END token. If it's not a final
            statement it will default to the Empty statement and the
            current token should be an END token which will break
            the loop and return all the statements. It could also be
            an Empty statement in the list of statements in which case
            the next token would be a SEMI.

            Was working through the logic for the Emtpy statement and 
            wanted to include some commentary on how/why it works."""
            return self.empty_statement()

    def assign_statement(self):

        left = self.variable()

        token = self.cur_token
        self.check_token_type(ASSIGN)

        right = self.expr1()
        
        return Assign(left, token, right)        

    def empty_statement(self):
        return Empty()

    def expr1(self):
        node = self.expr2()

        while self.cur_token.type in (ADD, SUB):
            token = self.cur_token
            if token.type == ADD:
                self.check_token_type(ADD)
            if token.type == SUB:
                self.check_token_type(SUB)

            node = BinOp(node, token, self.expr2())

        return node

    def expr2(self):
        node = self.expr3()

        while self.cur_token.type in (MUL, REAL_DIV, INT_DIV):
            token = self.cur_token
            if token.type == MUL:
                self.check_token_type(MUL)
            if token.type == INT_DIV:
                self.check_token_type(INT_DIV)
            if token.type == REAL_DIV:
                self.check_token_type(REAL_DIV)

            node = BinOp(node, token, self.expr3())

        return node

    def expr3(self):
        token = self.cur_token

        if token.type == ADD:
            self.check_token_type(ADD)
            return UnaryOp(token, self.expr3())
        if token.type == SUB:
            self.check_token_type(SUB)
            return UnaryOp(token, self.expr3())
        if token.type == INT_CONST:
            self.check_token_type(INT_CONST)
            return Num(token)
        if token.type == REAL_CONST:
            self.check_token_type(REAL_CONST)
            return Num(token)
        if token.type == OPAR:
            self.check_token_type(OPAR)
            return self.expr1()
            self.check_token_type(CPAR)
        else:
            return self.variable()


    def variable(self):
        var = Variable(self.cur_token)
        self.check_token_type(ID)
        return var

    def parse(self):
        root = self.program()
        return root

class NodeVisitor(object):

    def visit(self, node):
        visitor_name = 'visit_' + type(node).__name__ #method name
        visitor = getattr(self, visitor_name, self.visit_default) #method call
        return visitor(node)

    def visit_default(self, node):
        raise AttributeError('No class method visit_' + type(node).__name__)

class BuildAST(NodeVisitor):

    def __init__(self, parser):
        self.parser = parser
        self.loc = 0
        self.tree = None
        self.line1 = None
        self.line2 = None

    def visit_Program(self, node):
        pass

    def visit_Block(self, node):
        pass

    def visit_VarDecl(self, node):
        pass

    def visit_TypeSpec(self, node):
        pass

    def visit_Compound(self, node):
        pass

    def visit_Assign(self, node):
        pass

    def visit_Empty(self, node):
        pass

    def visit_BinOp(self, node):
        pass

    def visit_UnaryOp(self, node):
        pass

    def visit_Variable(self, node):
        pass

    def visit_Num(self, node):
        pass

    def build_ast(self):
        tree = self.parser.parse()
        if tree == None:
            return 'No tree.'
        else:
            print(tree)

class Interpreter(NodeVisitor):

    GLOBAL_SCOPE = {}

    def __init__(self, parser):
        self.parser = parser

    def visit_Program(self, node):
        self.visit(node.block)

    def visit_Block(self, node):
        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_VarDecl(self, node):
        pass

    def visit_TypeSpec(self, node):
        pass

    def visit_Compound(self, node):
        #pdb.set_trace()
        for statement in node.statements:
            self.visit(statement)

    def visit_Assign(self, node):
        var_name = node.left.value
        self.GLOBAL_SCOPE[var_name] = self.visit(node.right)

    def visit_Empty(self, node):
        pass

    def visit_BinOp(self, node):
        #pdb.set_trace()
        left = node.left
        op = node.op.type
        right = node.right

        if op == ADD:
            return self.visit(left) + self.visit(right)
        if op == SUB:
            return self.visit(left) - self.visit(right)
        if op == MUL:
            return self.visit(left) * self.visit(right)
        if op == INT_DIV:
            return self.visit(left) // self.visit(right)
        if op == REAL_DIV:
            return self.visit(left) / self.visit(right)

    def visit_UnaryOp(self, node):
        op = node.op.type
        if op == ADD:
            return self.visit(node.factor)
        if op == SUB:
            return -1*self.visit(node.factor)

    def visit_Variable(self, node):
        var_name = node.value
        value = self.GLOBAL_SCOPE.get(var_name)
        if value == None:
            raise NameError('Variable not defined.')
        else:
            return value

    def visit_Num(self, node):
        return node.value

    def interpret(self):
        root = self.parser.parse()
        if root == None:
            return ''
        else:
            return self.visit(root)
       

def main():
    text = open(sys.argv[1], 'r').read()
    
    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    #pdb.set_trace()
    interpreter.interpret()
    print(interpreter.GLOBAL_SCOPE)

if __name__ == "__main__":
    main()
