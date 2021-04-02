
"""

These classes are largely the same as previous lessons.

Updates include: 
1. Extending the lexer to recogonize Procedure Declarations
which may have been done prior to this lesson. 

2. Adding a Procedure and Param classes for the AST structure.

3. Adding methods to the Parser class to process the Procedures
and add Procedure and Param instances to the AST.

4. Adding a ProcedureSymbol class for the ScopedSymbolTable.

"""

from collections import namedtuple
from string import whitespace
import pdb

ADD         = 'ADD'
SUB         = 'SUB'
MUL         = 'MUL'
INT_DIV     = 'INT_DIV'
REAL_DIV    = 'REAL_DIV'
OPAR        = 'OPAR'
CPAR        = 'CPAR'

DOT         = 'DOT'
COMMA       = 'COMMA'
SEMI        = 'SEMI'
COLON       = 'COLON'
ASSIGN      = 'ASSIGN'

PROGRAM     = 'PROGRAM'
PROCEDURE   = 'PROCEDURE'
BEGIN       = 'BEGIN'
END         = 'END'
EOF         = 'EOF'

VAR         = 'VAR'
INT_TYPE    = 'INT_TYPE'
REAL_TYPE   = 'REAL_TYPE'
ID          = 'ID'
INT_CONST   = 'INT_CONST'
REAL_CONST  = 'REAL_CONST'

Token = namedtuple('Token', ['type', 'value'])

RESERVED_KEYWORDS = {
    'PROGRAM'   : Token(PROGRAM, 'PROGRAM'),
    'PROCEDURE' : Token(PROCEDURE, 'PROCEDURE'),
    'BEGIN'     : Token(BEGIN, 'BEGIN'),
    'END'       : Token(END, 'END'),
    'DIV'       : Token(INT_DIV, 'DIV'),
    'INTEGER'   : Token(INT_TYPE, 'INTEGER'),
    'REAL'      : Token(REAL_TYPE, 'REAL'),
    'VAR'       : Token(VAR, 'VAR')
    }





class Lexer(object):

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]

    def error(self):
        raise ValueError('Character string not recognized.')

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
        while self.cur_char != None and self.cur_char in whitespace:
            value = value + self.cur_char
            self.get_next_char()

    def get_comment(self):
        value = ''
        while self.cur_char != None and self.cur_char != '}':
            value = value + self.cur_char
            self.get_next_char()

        #get ending '}'
        value = value + self.cur_char
        self.get_next_char()

    def get_num(self):
        value = ''
        while self.cur_char != None and self.cur_char.isdigit(): 

            value = value + self.cur_char
            self.get_next_char()

            if self.cur_char == '.':
                value = value + self.cur_char
                self.get_next_char()
                while self.cur_char != None and self.cur_char.isdigit():
                    value = value + self.cur_char
                    self.get_next_char()

                return Token(REAL_CONST, float(value))

        return Token(INT_CONST, int(value))

    def get_decimal(self):
        value = self.cur_char
        self.get_next_char()
        while self.cur_char != None and self.cur_char.isdigit():
            value = value + self.cur_char
            self.get_next_char()

        return Token(REAL_CONST, float(value))
            
    def get_id(self):
        value = ''
        while (self.cur_char != None and 
            (self.cur_char.isalnum() or self.cur_char == '_')):

            value = value + self.cur_char
            self.get_next_char()

        #not case sensitive
        value = value.upper()

        return RESERVED_KEYWORDS.get(value, Token(ID, value))

    def get_next_token(self):

        symbol_dict = {
            '+' : Token(ADD, '+'),
            '-' : Token(SUB, '-'),
            '*' : Token(MUL, '*'),
            '/' : Token(REAL_DIV, '/'),
            '(' : Token(OPAR, '('),
            ')' : Token(CPAR, ')'),
            '.' : Token(DOT, '.'),
            ',' : Token(COMMA, ','),
            ';' : Token(SEMI, ';'),
            ':' : Token(COLON, ':')
        }

        while self.cur_char != None:

            if self.cur_char in whitespace:
                self.get_whitespace()
                continue
            if self.cur_char == '{':
                self.get_comment()
                continue
            if self.cur_char == ':' and self.peek_next_char() == '=':
                self.get_next_char()
                self.get_next_char()
                return Token(ASSIGN, ':=')
            if self.cur_char.isdigit():
                return self.get_num() 
            if self.cur_char == '.' and self.peek_next_char().isdigit():
                return self.get_decimal()
            if self.cur_char.isalpha() or self.cur_char == '_':
                return self.get_id()
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

class Procedure(AST):

    def __init__(self, name, block, params):
        self.name = name
        self.block = block
        self.params = params

class Param(AST):

    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

class VarDecl(AST):

    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

class Variable(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value

class TypeSpec(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value

class Compound(AST):

    def __init__(self):
        self.statements = []

class Assign(AST):

    def __init__(self, left, token, right):
        self.left = left
        self.token = self.op = token
        self.right = right

class BinOp(AST):

    def __init__(self, left, token, right):
        self.left = left
        self.token = self.op = token
        self.right = right

class UnaryOp(AST):

    def __init__(self, left, token):
        self.left = self.op = left
        self.token = self.factor = token

class Num(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value

class Empty(AST):
    pass





class Parser(object):

    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise SyntaxError('Syntax not recognized.')

    def check_token_type(self, token_type):
        print(self.cur_token)
        if self.cur_token.type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error()

    def program(self):
        """program: PROGRAM variable SEMI block DOT"""

        self.check_token_type(PROGRAM)

        name = self.cur_token.value
        self.check_token_type(ID)

        self.check_token_type(SEMI)

        block = self.block()

        self.check_token_type(DOT)

        return Program(name, block)

    def block(self):
        """block : declarations compound_statement"""
        declarations = self.declarations()

        compound_statements = self.compound_statement()

        return Block(declarations, compound_statements)

    def declarations(self):
        """declarations : (VAR (variable_declaration SEMI)+)* 
                        | (PROCEDURE ID (OPAR formal_param_list CPAR)? SEMI block SEMI)*
                        | empty
        """
        decls = []

        while True:
            if self.cur_token.type == VAR:
                self.check_token_type(VAR)
                while self.cur_token.type == ID:
                    decls.extend(self.var_decl())
                    self.check_token_type(SEMI)

            elif self.cur_token.type == PROCEDURE:
                self.check_token_type(PROCEDURE)

                name = self.cur_token.value
                self.check_token_type(ID)
            
                params = [] 
                if self.cur_token.type == OPAR:
                    self.check_token_type(OPAR)
                    params = self.formal_param_list()
                    self.check_token_type(CPAR)

                self.check_token_type(SEMI)

                block = self.block()

                self.check_token_type(SEMI)

                decls.append(Procedure(name, block, params))
            else:
                break

        return decls


    def formal_param_list(self):
        """formal_param_list : formal_param 
                             | formal_param SEMI formal_param_list
        """

        #didn't think about the possibility of having parathesis with
        #no parameters
        if not self.cur_token.type == ID:
            return []

        #formal_param returns a list
        fpl = self.formal_param()
        
        while self.cur_token.type == SEMI:
            self.check_token_type(SEMI)
            fpl.extend(self.formal_param())

        if self.cur_token.type == ID:
            self.error()

        return fpl

    def formal_param(self):
        """formal_param : ID (COMMA ID)* COLON type_spec"""

        var_nodes = [self.variable()]

        while self.cur_token.type == COMMA:
            self.get_next_token(COMMA)
            var_nodes.append(self.variable())

        self.check_token_type(COLON)

        type_node = self.type_spec()

        fps = []
        for var_node in var_nodes:
            fps.append(Param(var_node, type_node))

        return fps

    def var_decl(self):
        """ variable_declaration : ID (COMMA ID)* COLON type_spec"""

        var_nodes = [self.variable()]

        while self.cur_token.type == COMMA:
            self.check_token_type(COMMA)
            var_nodes.append(self.variable())

        self.check_token_type(COLON)

        type_node = self.type_spec()

        var_decls = []
        for var_node in var_nodes:
            var_decls.append(VarDecl(var_node, type_node))

        return var_decls

    def type_spec(self):
        """ type_spec : INTEGER | REAL"""

        token = self.cur_token

        if token.type == INT_TYPE:
            self.check_token_type(INT_TYPE)
        if token.type == REAL_TYPE:
            self.check_token_type(REAL_TYPE)

        return TypeSpec(token)

    def compound_statement(self):
        """compound_statement : BEGIN statement_list END"""

        self.check_token_type(BEGIN)
        statement_list = self.statement_list()
        self.check_token_type(END)

        compound = Compound()
        for statement in statement_list:
            compound.statements.append(statement)

        return compound

    def statement_list(self):
        """statement_list : statement
                          | statement SEMI statement_list
        """
        statement_list = [self.statement()]

        while self.cur_token.type == SEMI:
            self.check_token_type(SEMI)
            statement_list.append(self.statement())

        #checks against an additional illegal following statment
        if self.cur_token.type == ID:
            self.error()

        return statement_list

    def statement(self):
        """statement : compound_statement
                     | assignment_statement
                     | empty
        """

        token = self.cur_token

        if token.type == BEGIN:
            return self.compound_statement()
        if token.type == ID:
            return self.assign_statement()
        else:
            return self.empty()

    def assign_statement(self):
        """assignment_statement : variable ASSIGN expr1"""

        left = self.variable()

        op = self.cur_token
        self.check_token_type(ASSIGN)

        right = self.expr1()

        return Assign(left, op, right) 

    def empty(self):
        """empty : """
        return Empty()

    def variable(self):
        """variable : ID"""
        token = self.cur_token
        self.check_token_type(ID)
        return Variable(token)

    def expr1(self):
        """expr1 : expr2 ((ADD | SUB) expr2)*"""

        node = self.expr2()

        while self.cur_token.type in (ADD,SUB):
            token = self.cur_token
            if token.type == ADD:
                self.check_token_type(ADD)
            if token.type == SUB:
                self.check_token_type(SUB)

            node = BinOp(node, token, self.expr2())

        return node

    def expr2(self):
        """expr2 : expr3 ((MUL | INT_DIV | REAL_DIV) expr3)*"""
        
        node = self.expr3()

        while self.cur_token.type in (MUL,INT_DIV,REAL_DIV):
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
        """expr3 : ADD expr3
                 | SUB expr3
                 | INT_CONST
                 | REAL_CONST
                 | OPAR expr1 CPAR
                 | variable
        """

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
        if token.type == ID:
            return self.variable()

    def parse(self):
        root = self.program()
        if root == None:
            return None
        return root




class Symbol(object):
    
    def __init__(self, name, type=None):
        self.name = name
        self.type = type

class VarSymbol(Symbol):

    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return f'<{self.__class__.__name__}(name={self.name}, type={self.type})>'

    __repr__ = __str__

class ProcedureSymbol(Symbol):

    def __init__(self, name, params=None):
        super().__init__(name)
        self.params = params if params is not None else []

    def __str__(self):
        return f'<{self.__class__.__name__}(name={self.name}, parameters={self.params})>'

    __repr__ = __str__

class BuiltinTypeSymbol(Symbol):

    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return self.name

    def __repr(self):
        return f'<{self.__class__.__name__}(name={self.name})'







class NodeVisitor(object):

    def visit(self, node):
        visitor_name = 'visit_' + type(node).__name__
        visitor = getattr(self, visitor_name, self.visitor_default)
        return visitor(node)

    def visitor_default(self, node):
        raise Exception('No method name: visit_' + type(node).__name__)









class VisualAST(NodeVisitor):

    def __init__(self, root_node):
        self.root_node = root_node

    def visit_Program(self, node):
        print(type(node).__name__)

        print(node.name)
        self.visit(node.block)

    def visit_Block(self, node):
        print(type(node).__name__)

        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)        

    def visit_Procedure(self, node):
        print(type(node).__name__)

        print(node.name)

        for param in node.params:
            self.visit(param)

        self.visit(node.block)

    def visit_Param(self, node):
        print(type(node).__name__)

        self.visit(node.var_node)
        self.visit(node.type_node)

    def visit_VarDecl(self, node):
        print(type(node).__name__)

        self.visit(node.var_node)
        self.visit(node.type_node)

    def visit_Variable(self, node):
        print(type(node).__name__)

        print(node.value)

    def visit_TypeSpec(self, node):
        print(type(node).__name__)

        print(node.value)

    def visit_Compound(self, node):
        print(type(node).__name__)

        for statement in node.statements:
            self.visit(statement)

    def visit_Assign(self, node):
        print(type(node).__name__)

        self.visit(node.left)

        print(node.op)

        self.visit(node.right)

    def visit_BinOp(self, node):
        print(type(node).__name__)

        self.visit(node.left)

        print(node.op)

        self.visit(node.right)

    def visit_UnaryOp(self, node):
        print(type(node).__name__)

        print(node.op)

        self.visit(node.right)

    def visit_Num(self, node):
        print(type(node).__name__)

        print(node.value)

    def visit_Empty(self, node):
        print(type(node).__name__)

        print('Empty')

    def build_ast(self):
        self.visit(self.root_node)
