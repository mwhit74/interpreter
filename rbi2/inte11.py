
from collections import OrderedDict, namedtuple
from string import whitespace
import pdb
import sys

ADD         = 'ADD'
SUB         = 'SUB'
MUL         = 'MUL'
INT_DIV     = 'INT_DIV'
REAL_DIV    = 'REAL_DIV'
DOT         = 'DOT'
SEMI        = 'SEMI'
COLON       = 'COLON'
COMMA       = 'COMMA'
PROGRAM     = 'PROGRAM'
INT_CONST   = 'INT_CONST'
REAL_CONST  = 'REAL_CONST'
INT_TYPE    = 'INT_TYPE'
REAL_TYPE   = 'REAL_TYPE'
BEGIN       = 'BEGIN'
END         = 'END'
ID          = 'ID'
VAR         = 'VAR'
ASSIGN      = 'ASSIGN'
OPAR        = 'OPAR'
CPAR        = 'CPAR'
EOF         = 'EOF'

Token = namedtuple('Token', ['type', 'value'])

RESERVED_KEYWORDS = {
    'BEGIN'     : Token(BEGIN, 'BEGIN'),
    'END'       : Token(END, 'END'),
    'INTEGER'   : Token(INT_TYPE, 'INTEGER'),
    'REAL'      : Token(REAL_TYPE, 'REAL'),
    'PROGRAM'   : Token(PROGRAM, 'PROGRAM'),
    'DIV'       : Token(INT_DIV, 'DIV'),
    'VAR'       : Token(VAR, 'VAR'),
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
        while self.cur_char != None and self.cur_char in whitespace:
            value = value + self.cur_char
            self.get_next_char()

    def get_comment(self):
        value = ''
        while self.cur_char != '}':
            value = value + self.cur_char
            self.get_next_char()

        #gets closing '}'
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
                #pdb.set_trace()

                while self.cur_char != None and self.cur_char.isdigit():
                    value = value + self.cur_char
                    self.get_next_char()

                return Token(REAL_CONST, float(value))
            
        return Token(INT_CONST, int(value))

    def get_id(self):
        value = ''
        while (self.cur_char != None and 
            (self.cur_char.isalnum() or self.cur_char == '_')):
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
            ',':Token(COMMA, ','),
            '.':Token(DOT, '.'),
            ';':Token(SEMI, ';'),
            ':':Token(COLON, ':'),
            '(':Token(OPAR, '('),
            ')':Token(CPAR, ')')
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

class VarDecl(AST):

    def __init__(self, type_node, var_node):
        self.type_node = type_node
        self.var_node = var_node

class Compound(AST):

    def __init__(self):
        self.statement_list = []
        
class Assign(AST):

    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class Empty(AST):
    pass

class TypeSpec(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value

class Variable(AST):
    
    def __init__(self, token):
        self.token = token
        self.value = self.token.value

class Num(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value

class BinOp(AST):

    def __init__(self, left, token, right):
        self.left = left
        self.token = self.op = token
        self.right = right

class UnaryOp(AST):

    def __init__(self, left, factor):
        self.left = self.op = left
        self.factor = factor

class Parser(object):

    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise NameError('Token not recognized.')

    def check_token_type(self, token_type):
        print(self.cur_token)
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
        self.check_token_type(EOF)

        return Program(name, block)

    def block(self):
        declarations = self.declarations()
        statements = self.compound_statement()

        return Block(declarations, statements)

    def declarations(self):
        decls = []
    
        if self.cur_token.type == VAR:
            self.check_token_type(VAR)
            while self.cur_token.type == ID:
                decls.extend(self.var_decl())
                self.check_token_type(SEMI)

        return decls
            
    def var_decl(self):
        var_nodes = [self.variable()]
        
        while self.cur_token.type == COMMA:
            self.check_token_type(COMMA)
            var_nodes.append(self.variable())

        self.check_token_type(COLON)

        type_node = self.type_spec()
            
        var_decls = []
        for var_node in var_nodes:
            var_decls.append(VarDecl(type_node, var_node))

        return var_decls

    def type_spec(self):
        token = self.cur_token

        if token.type == INT_TYPE:
            self.check_token_type(INT_TYPE)
        if token.type == REAL_TYPE:
            self.check_token_type(REAL_TYPE)

        return TypeSpec(token)

    def compound_statement(self):
        self.check_token_type(BEGIN)
        statements = self.statement_list()
        self.check_token_type(END)

        compound = Compound()
        for statement in statements:
            compound.statement_list.append(statement)

        return compound

    def statement_list(self):
        statements = [self.statement()]

        while self.cur_token.type == SEMI:
            self.check_token_type(SEMI)
            statements.append(self.statement())

        if self.cur_token.type == ID:
            self.error()

        return statements

    def statement(self):
        token = self.cur_token

        if token.type == BEGIN:
            return self.compound_statement()
        if token.type == ID:
            return self.assign()
        else:
            #oaf, this error was hard to track down
            #I honestly don't know if I would have figured it
            #out without looking back at other correct code
            #Empty statements are *not* tokenized. Therefore, the 
            #above statement function must default to creating an Empty
            #node. It cannot test the token for an EMPTY type which is
            #what caused the bug. 
            return self.empty()

    def assign(self):
        #pdb.set_trace()
        left = self.variable()
        
        token = self.cur_token
        self.check_token_type(ASSIGN)

        right = self.expr1()

        return Assign(left, token, right)
        
    def empty(self):
        return Empty()

    def expr1(self):
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
        node = self.expr3()

        while self.cur_token.type in (MUL, REAL_DIV, INT_DIV):
            token = self.cur_token
            if token.type == MUL:
                self.check_token_type(MUL)
            if token.type == REAL_DIV:
                self.check_token_type(REAL_DIV)
            if token.type == INT_DIV:
                self.check_token_type(INT_DIV)

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
        if token.type == OPAR:
            self.check_token_type(OPAR)
            node = self.expr1()
            self.check_token_type(CPAR)
            return node
        if token.type == REAL_CONST:
            num = Num(token)
            self.check_token_type(REAL_CONST)
            return num
        if token.type == INT_CONST:
            num = Num(token)
            self.check_token_type(INT_CONST)
            return num
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
        visitor_name = 'visit_' + type(node).__name__
        visitor = getattr(self, visitor_name, self.visitor_default)
        return visitor(node) 

    def visitor_default(self, node):
        raise NameError('No method named visit_' + type(node).__name__)

class Interpreter(NodeVisitor):

    GLOBAL_SCOPE = {}

    def __init__(self, root_node):
        self.root_node = root_node

    def error(self):
        raise SyntaxError('Syntax not recognized.')

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
        for statement in node.statement_list:
            #pdb.set_trace()
            self.visit(statement)

    def visit_Assign(self, node):
        var_name = node.left.value
        self.GLOBAL_SCOPE[var_name] = self.visit(node.right)

    def visit_Empty(self, node):
        pass

    def visit_BinOp(self, node):
        op_type = node.op.type
        left = node.left
        right = node.right

        if op_type == ADD:
            return self.visit(left) + self.visit(right)
        if op_type == SUB:
            return self.visit(left) - self.visit(right)
        if op_type == MUL:
            return self.visit(left) * self.visit(right)
        if op_type == INT_DIV:
            return self.visit(left) // self.visit(right)
        if op_type == REAL_DIV:
            return self.visit(left) / self.visit(right)

    def visit_UnaryOp(self, node):
        op_type = node.op.type

        if op_type == ADD:
            return (+1)*self.visit(node.factor)
        if op_type == SUB:
            return (-1)*self.visit(node.factor)

    def visit_Variable(self, node):
        var_name = node.value
        var_value = self.GLOBAL_SCOPE.get(var_name)
        if var_value == None:
            raise NameError('Variable not defined.')
        else:
            return var_value

    def visit_Num(self, node):
        return node.value

    def interpret(self):
        self.visit(self.root_node)


class BuildAST(NodeVisitor):

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

    def visit_VarDecl(self, node):
        print(type(node).__name__)
        self.visit(node.type_node)
        self.visit(node.var_node)

    def visit_TypeSpec(self, node):
        print(type(node).__name__)
        print(node.value)

    def visit_Compound(self, node):
        print(type(node).__name__)
        for statement in node.statement_list:
            #pdb.set_trace()
            self.visit(statement)

    def visit_Assign(self, node):
        print(type(node).__name__)
        self.visit(node.left)
        print(node.op)
        self.visit(node.right)

    def visit_Empty(self, node):
        print(type(node).__name__)

    def visit_BinOp(self, node):
        print(type(node).__name__)

        op_type = node.op.type
        left = node.left
        right = node.right

        if op_type == ADD:
            print(ADD)
            self.visit(left)
            self.visit(right)
        if op_type == SUB:
            print(SUB)
            self.visit(left)
            self.visit(right)
        if op_type == MUL:
            print(MUL)
            self.visit(left)
            self.visit(right)
        if op_type == REAL_DIV:
            print(REAL_DIV)
            self.visit(left)
            self.visit(right)
        if op_type == INT_DIV:
            print(INT_DIV)
            self.visit(left)
            self.visit(right)
    
    def visit_UnaryOp(self, node):
        print(type(node).__name__)

        op_type = node.op.type

        if op_type == ADD:
            print(ADD)
            self.visit(node.factor)
        if op_type == SUB:
            print(SUB)
            self.visit(node.factor)

    def visit_Variable(self, node):
        print(type(node).__name__)
        print(node.value)

    def visit_Num(self, node):
        print(type(node).__name__)
        print(node.value)

    def build_ast(self):
        self.visit(self.root_node)


class Symbol(object):

    def __init__(self, name, type=None):
        self.name = name
        self.type = type

class BuiltinTypeSymbol(Symbol):

    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return self.name

    __repr__ = __str__

class VarSymbol(Symbol):

    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return '<{name}:{type}>'.format(name=self.name, type=self.type)

    __repr__ = __str__

class SymbolTable(object):

    def __init__(self):
        self._symbols = OrderedDict()
        self._init_builtins()

    def _init_builtins(self):
        self.define(BuiltinTypeSymbol('INTEGER'))
        self.define(BuiltinTypeSymbol('REAL'))

    def __str__(self):
        symbols = [value for value in self._symbols.values()]
        s = 'Symbols:{0}'.format(symbols)
        return s

    def define(self, symbol):
        print('Define ' + str(symbol))
        self._symbols[symbol.name] = symbol

    def lookup(self, name):
        print('Lookup ' + str(name))
        symbol = self._symbols.get(name)
        return symbol

class SymbolTableBuilder(NodeVisitor):

    def __init__(self, root_node):
        self.symtab = SymbolTable()
        self.root_node = root_node

    def visit_Program(self, node):
        self.visit(node.block)

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)

        self.visit(node.compound_statements)

    def visit_VarDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.symtab.lookup(type_name) #gets built-in type symbol
        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)
        self.symtab.define(var_symbol)

    def visit_Compound(self, node):
        for statement in node.statement_list:
            self.visit(statement)

    def visit_Assign(self, node):
        #checks that the assignment variable has been declared
        var_name = node.left.value
        var_symbol = self.symtab.lookup(var_name)
        if var_symbol is None:
            raise NameError(repr(var_name))

        self.visit(node.right)
        

    def visit_Empty(self, node):
        pass

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_UnaryOp(self, node):
        self.visit(node.factor)

    def visit_Num(self, node):
        pass

    def visit_Variable(self, node):
        #checks that a variable in an expression has been declared
        var_name = node.value
        var_symbol = self.symtab.lookup(var_name)

        if var_symbol is None:
            raise NameError(repr(var_name))

    def type_check(self):
        self.visit(self.root_node)
        


def main():
    text = open(sys.argv[1], 'r').read()

    lexer = Lexer(text)
    parser = Parser(lexer)
    root_node = parser.parse()

    visual_ast = BuildAST(root_node)
    visual_ast.build_ast()

    symtab = SymbolTableBuilder(root_node)
    symtab.type_check()
   
    interpreter = Interpreter(root_node)
    interpreter.interpret()

    print(interpreter.GLOBAL_SCOPE)

if __name__ == '__main__':
    main() 
