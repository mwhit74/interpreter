
import string
import pdb
from collections import namedtuple

ADD        = 'ADD'
SUB        = 'SUB'
MUL        = 'MUL'
INT_DIV    = 'INT_DIV'
FLOAT_DIV  = 'FLOAT_DIV'
INT        = 'INT' #used for variable type declaration, type_spec
REAL       = 'REAL' #used for variable type declartion, type_spec
INT_CONST  = 'INT_CONST' #used for number token
REAL_CONST = 'REAL_CONST' #used for number token
OPAR       = 'OPAR'
CPAR       = 'CPAR'
ASSIGN     = 'ASSIGN'
ID         = 'ID'
BEGIN      = 'BEGIN'
END        = 'END'
PROGRAM    = 'PROGRAM'
DOT        = 'DOT'
SEMI       = 'SEMI'
COLON      = 'COLON'
VAR        = 'VAR'
EOF        = 'EOF'


Token = namedtuple('Token', ['type', 'value'])

#language keyword : interpreter internal token representation
RESERVED_KEYWORDS = {
    'PROGRAM' : Token('PROGRAM', 'PROGRAM'),
    'VAR' : Token('VAR', 'VAR'),
    'DIV' : Token('INT_DIV', 'DIV'),
    'INTEGER' : Token('INT', 'INT'),
    'REAL' : Token('REAL', 'REAL'),
    'BEGIN' : Token('BEGIN', 'BEGIN'),
    'END' : Token('END', 'END'),
}


class Lexer(object):

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]

    def error(self):
        raise ValueError("Character not recognized.")

    def get_next_char(self):
        self.pos += 1
        if self.pos <= len(text) - 1:
            self.cur_char = self.text[self.pos]
        else:
            self.cur_char = None

    def peek_next_char(self):
        pos = self.pos + 1
        if pos <= len(text) - 1:
            return self.text[pos]
        else:
            return None

    def get_whitespace(self):
        value = ''
        while self.cur_char != None and self.cur_char.isspace():
            value = value + self.cur_char
            self.get_next_char()

    def get_comment(self):
        value = ''
        while self.cur_char != None and self.cur_char != '}':
            value = value + self.cur_char
            self.get_next_char()

        #gets the end curly bracket
        value = value + self.cur_char
        self.get_next_char()

    def get_num(self):
        value = ''
        while self.cur_char != None and self.cur_char.isdigit():
            value = value + self.cur_char()
            self.get_next_char()

        if self.cur_char == '.':
            value = value + self.cur_char()
            self.get_next_char()

            while self.cur_char != None and self.cur_char.isdigit():
                value = value + self.cur_char()
                self.get_next_char()

            token = Token(REAL_CONST, float(value))

        else:
            token = token(INT_CONST, int(value))

        return token

    def get_char(self):
        value = ''
        while (self.cur_char != None and 
                (self.cur_char.isalnum() or self.cur_char == '_')):
    
            value = value + self.cur_char
            self.get_next_char()

        return RESERVED_KEYWORDS.get(value, Token(ID, value))

    def get_next_token(self):
        op_dict = {'+':Token(ADD, '+'),
                   '-':Token(SUB, '-'),
                   '*':Token(MUL, '*'),
                   '/':Token(FLOAT_DIV, '/'),
                   '(':Token(OPAR, '('),
                   ')':Token(CPAR, ')'),
                   ':':Token(COLON, ':'),
                   ';':Token(SEMI, ';'),
                   '.':Token(DOT, '.'),
                   ',':Token(COMMA, ',')
                  }
                   

        while cur_char != None:
            if self.cur_char.isspace():
                self.get_whitespace()
            if self.cur_char == '{':
                self.get_comment()
                continue
            if self.cur_char.isdigit():
                token = self.get_num()
                return token
            if self.cur_char == ":" and self.peek_next_char() == "=":
                self.get_next_char()
                self.get_next_char()
                return Token(ASSIGN, ':=')
            if self.cur_char.isalpha() or self.cur_char == '_':
                token = self.get_char()
                return token
            if self.cur_char in list(op_dict):
                token = op_dict[self.cur_char]
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
    
    def __init__(self, declarations, compound_statement):
        self.declarations = declarations
        self.compound_statement = compound_statement

class VarDecl(AST):

    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

class Type(AST):

    def __init__(self, token):
        self.token = token
        self.value = token.value

class BinOp(AST):

    def __init__(self, right, token, left):
        self.right = right
        self.token = self.op = token
        self.left = left

class UnaryOp(AST):

    def __init__(self, token, expr):
        self.token = self.op = token
        self.factor = expr

class Num(AST):

    def __init__(self, token):
        self.token = token
        self.value = token.value

class Compound(AST):

    def __init__(self):
        self.children = []

class Assign(AST):

    def __init__(self, left, token, right):
        self.left = left
        self.token = self.op = token
        self.right = right

class Var(AST):

    def __init__(self, token):
        self.token = token
        self.value = token.value

class Empty(AST):
    pass

class Parser(object):

    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise SyntaxError('Syntax not recognized.')
    
    def check_token_type(self, token_type):
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
        """block: declarations compound_statement"""

        declarations = self.declarations()
        compound_statements = self.compound_statements()
        return Block(declarations, compound_statements)

    def declarations(self):
        """declarations: VAR (variable_declaration SEMI)+ | empty"""

        """The VAR token is only an identifier, a keyword with no attional meaning
            other than the structure of the program. Therefore, it can just
            be checked as a program organizational feature."""

        if self.cur_token_type == VAR
            self.check_token_type(VAR)
            declarations = []
            while self.cur_token.type == ID:
                declarations.extend(self.var_dec())
                self.check_token_type(SEMI)
            return declarations

    def var_dec(self):
        """variable declaration: ID (COMMA ID)* COLON type_spec"""

        """The ID token has additional meaning, it actually holds a value,
            in this case a variable, that needs additional processing and 
            representation in the AST. This is in contrast to the VAR token."""

        var_nodes = [Var(self.cur_token)]
        self.check_token_type(ID)

        while self.cur_token.type == COMMA:
            self.check_token_type(COMMA)
            var_nodes.append(Var(self.cur_token))
            self.check_token_type(ID)
            
        self.check_token_type(COLON)

        type_node = self.type_spec()

        var_declarations = [
            VarDecl(var_node, type_node)
            for var_node in var_nodes
        ]

        return var_declarations

    def type_spec(self):
        """type_spec: INT | REAL"""

        if self.token.value == INT:
            token = self.cur_token
            self.check_token_type(INT)
        else:
            token = self.cur_token
            self.check_token_type(REAL)

        return Type(token)

    def compound_statements(self):
        """compound_staetment : BEING statement_list END"""

        self.check_token_type(BEGIN)
        nodes = self.statement_list()
        self.check_token_type(END)

        compound_node = Compound()
        for node in nodes:
            compound_node.children.append(node)

        return compound_node

    def statement_list(self):
        """statement_list : statement | 
                            statement SEMI statement_list"""

        statement_list = [self.statement()]
        
        while self.cur_token.type == SEMI:
            self.check_token_type(SEMI)
            statement_list.append(self.statement())

        #checks against missing SEMI between statements
        if self.cur_token.type == ID:
            self.error()

        return statement_list

    def statement(self):
        """statement: compound_statement |
                      assignment_statement |
                      empty
        """

        if self.cur_token.type == BEGIN:
            return self.compound_statements()
        elif self.cur_token.type == ASSIGN:
            return self.assignment_statement()
        else:
            return self.empty_statement()

    def assignment_statement(self):
        """assignment_statement: variable ASSIGN expr"""

        left = self.variable()
        self.check_token_type(ID)

        op = self.cur_token
        self.check_token_type(ASSIGN)

        right = self.expr1()

        return Assign(left, op, right)


    def empty_statement(self):
        return Empty()

    def expr1(self):
        """expr1: expr2 ((ADD | SUB) expr2)* """
        node = self.expr2()

        while (self.cur_token.type != None 
                and self.cur_token.type in (ADD, SUB)):
    
            token = self.cur_token

            if self.cur_token.type == ADD:
                self.check_token_type(ADD)
            if self.cur_token.type == SUB:
                self.check_token_type(SUB)

            #the first node shares the same variable name as
            #subsequent nodes so if there are multiple additions
            #or subtractions the nodes get continually pushed down
            #the tree
            #the node assignment goes inside the while loop so if 
            #there are multiple consecutive adds or subs the nodes
            #get continually pushed down the tree
            #in short it creates the recusion structure to later be
            #be read by the interpreter
            node = BinOp(node, token, self.expr2())

        return node

    def expr2(self):
        """expr2: factor ((MUL | INT_DIV | FLOAT_DIV) factor)* """
        node = self.factor()

        while (self.cur_token.type != None
                and self.cur_token.type in (MUL, INT_DIV, FLOAT_DIV)):

            token = self.cur_token

            if self.cur_token.type == MUL:
                self.check_token_type(MUL)
            if self.cur_token.type == INT_DIV:
                self.check_token_type(INT_DIV)
            if self.cur_token.type == FLOAT_DIV:
                self.check_token_type(FLOAT_DIV)

            node = BinOp(node, token, self.factor())

        return node

    def factor(self):
        """factor: ADD factor |
                   SUB factor |
                   INT_CONST |
                   REAL_CONST |
                   LPAREN expr1 RPAREN |
                   variable
        """

        token_type = self.cur_token.type

        if token_type == ADD:
            token = self.cur_token
            self.check_token_type(ADD)
            return UnaryOp(token, self.factor())
        if token_type == SUB:
            token = self.cur_token
            self.check_token_type(SUB)
            return UnaryOp(token, self.factor())
        if token_type == INT_CONST:
            token = self.cur_token
            self.check_token_type(INT_CONST)
            return Num(token)
        if token_type == REAL_CONST:
            token = self.cur_token
            self.check_token_type(REAL_CONST)
            return Num(token)
        if token_type == LPAREN:
            self.check_token_type(LPAREN)
            node = self.expr1()
            self.check_token_type(RPAREN)
            return node
        else:
            return self.variable()

    def variable(self):

        token = self.cur_token
        self.check_token_type(ID)
        return Var(token) 

    def parse(self):
        root = self.program()
        return root

class NodeVisitor(self, node)

    def visit(self, node):
        visit_name = 'visit_' + type(node).__name__
        visitor = getattr(self, visit_name, visit_default)
        return self.visitor(node)

    def visit_default(self, node)):
        raise Exception('Visit method ' + type(node).__name__ + 'does not exist.')


class Interpreter(NodeVisitor):

    GLOBAL_SCOPE = {}

    def __init__(self, parser):
        self.parser = parser)

    def visit_Program(self, node):
        self.visit(node.block)

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        pass

    def visit_Type(self, node):
        pass

    def visit_BinOp(self, node):

        if node.op.type == ADD:
            return self.visit(node.left) + self.visit(node.right)
        if node.op.type == SUB:
            return self.visit(node.left) - self.visit(node.right)
        if node.op.type == MUL:
            return self.visit(node.left) - self.visit(node.right)
        if node.op.type == INT_DIV:
            return self.visit(node.left) // self.visit(node.right)
        if node.op.type == FLOAT_DIV:
            return self.visit(node.left) / self.visit(node.right)

    def visit_UnaryOp(self, node):

        if node.op.type == ADD:
            return + self.visit(node.factor)
        if node.op.type == SUB:
            return - self.visit(node.factor)

    def visit_Compound(self, node):

        for child in node.children:
            return self.visit(child)

    def visit_Assign(self, node):

        self.GLOBAL_SCOPE[node.left.value] = self.visit(node.right)

    def visit_Num(self, node):

        return self.node.value

    def visit_Var(self, node):
        var_name = node.value
        value = self.GLOBAL_SCOPE.get(var_name)
        if value == NONE:
            raise NameError('Variable' + var_name + 'not defined.')
        else:
            return value

    def visit_Empty(self, node):
        pass

    def intepret(self):
        root = self.parser.parse()
        if root is None:
            return ''
        return self.visit(root)

def main():
    import sys
    
    text = open(sys.argv[1], 'r').read()

    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpretet(parser)
    result = interpreter.interpret()
    print(interpreter.GLOBAL_SCOPE) 


if __name__ == "__main__":
    main()
