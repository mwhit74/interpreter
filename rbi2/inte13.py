
from collections import OrderedDict, namedtuple
from string import whitespace
import pdb
import sys

ADD         = 'ADD'
SUB         = 'SUB'
MUL         = 'MUL'
INT_DIV     = 'INT_DIV'
REAL_DIV    = 'REAL_DIV'
INT_TYPE    = 'INT_TYPE'
REAL_TYPE   = 'REAL_TYPE'
INT_CONST   = 'INT_CONST'
REAL_CONST  = 'REAL_CONST'
DOT         = 'DOT'
COMMA       = 'COMMA'
SEMI        = 'SEMI'
COLON       = 'COLON'
OPAR        = 'OPAR'
CPAR        = 'CPAR'
VAR         = 'VAR'
ID          = 'ID'
BEGIN       = 'BEGIN'
END         = 'END'
ASSIGN      = 'ASSIGN'
PROGRAM     = 'PROGRAM'
PROCEDURE   = 'PROCEDURE'
EOF         = 'EOF'

Token = namedtuple('Token',['type', 'value'])

RESERVED_KEYWORDS = {
    'PROGRAM'    : Token(PROGRAM, 'PROGRAM'),
    'PROCEDURE'   : Token(PROCEDURE, 'PROCEDURE'),
    'VAR'         : Token(VAR, 'VAR'),
    'BEGIN'       : Token(BEGIN, 'BEGIN'),
    'END'         : Token(END, 'END'),
    'DIV'         : Token(INT_DIV, 'DIV'),
    'INTEGER'     : Token(INT_TYPE, 'INTEGER'),
    'REAL'        : Token(REAL_TYPE, 'REAL')
    }

class Lexer(object):

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]

    def error(self):
        raise NameError('Character not recogized.')
    
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

        #get trailing '}'
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
                while self.cur_char.isdigit():
                    value = value + self.cur_char
                    self.get_next_char()

                return Token(REAL_CONST, float(value))

        return Token(INT_CONST, int(value))

    def get_id(self):
        value = ''
        while (self.cur_char != None 
            and (self.cur_char.isalnum() or self.cur_char == '_')):
            value = value + self.cur_char
            self.get_next_char()

        #keywords and variables not case sensitive
        value = value.upper()

        return RESERVED_KEYWORDS.get(value, Token(ID, value))

    def get_next_token(self):
        op_dict = {
            '+':Token(ADD, '+'),
            '-':Token(SUB, '-'),
            '*':Token(MUL, '*'),
            '/':Token(REAL_DIV, '/'),
            '.':Token(DOT, '.'),
            ',':Token(COMMA, ','),
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

class Procedure(AST):

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
        self.value = self.token.value

class Variable(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value

class Compound(AST):

    def __init__(self):
        self.statement_list = []

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

    def __init__(self, left, factor):
        self.left = self.op = left
        self.factor = factor

class Empty(AST):
    pass

class Num(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value


class Parser(object):

    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise SyntaxError('Syntax not recognized.')

    def check_token_type(self, token_type):
        print(self.cur_token)
        #pdb.set_trace()
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

    def procedure(self):
        self.check_token_type(PROCEDURE)

        name = self.cur_token.value
        self.check_token_type(ID)

        self.check_token_type(SEMI)

        block = self.block()

        self.check_token_type(SEMI)

        return Procedure(name, block)

    def block(self):
        declarations = self.declaration()
        compound_statements = self.compound_statement()

        return Block(declarations, compound_statements)

    def declaration(self):
        var_decls = []

        if self.cur_token.type == VAR:
            self.check_token_type(VAR)
            while self.cur_token.type == ID:
                var_decls.extend(self.variable_declaration())
                self.check_token_type(SEMI)

        while self.cur_token.type == PROCEDURE:
            var_decls.append(self.procedure())

        return var_decls

    def variable_declaration(self):
        var_nodes = [self.variable()]

        while self.cur_token.type == COMMA:
            self.check_token_type(COMMA)
            var_nodes.append(self.variable())
            
        self.check_token_type(COLON)

        type_node = self.type_spec()

        var_decls = []
        var_decls = [VarDecl(type_node, var_node) for var_node in var_nodes]

        return var_decls

    def type_spec(self):
        token = self.cur_token

        if token.type == INT_TYPE:
            self.check_token_type(INT_TYPE)
        if token.type == REAL_TYPE:
            self.check_token_type(REAL_TYPE)

        return TypeSpec(token)

    def variable(self):
        token = self.cur_token
        self.check_token_type(ID)
        return Variable(token)

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
            return self.assign_statement()
        else:
            return self.empty()

    def assign_statement(self):
        left = self.variable()

        token = self.cur_token
        self.check_token_type(ASSIGN)

        right = self.expr1()

        return Assign(left, token, right)

    def empty(self):
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

        while self.cur_token.type in (MUL, INT_DIV, REAL_DIV):
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
            node = self.expr1()
            self.check_token_type(CPAR)
            return node
        if token.type == ID:
            return self.variable()

    def parse(self):
        tree = self.program()
        if tree == None:
            return ''
        return tree

class NodeVisitor(object):

    def visit(self, node):
        visitor_name = 'visit_' + type(node).__name__
        visitor = getattr(self, visitor_name, self.default_visitor)
        return visitor(node)

    def default_visitor(self, node):
        raise Exception('Method visit_' + type(node).__name__ + ' not found.')

class VisualAST(NodeVisitor):

    def __init__(self, ast):
        self.ast = ast

    def visit_Program(self, node):
        print(type(node).__name__)
        print(node.name)
        self.visit(node.block)

    def visit_Procedure(self, node):
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
        self.visit(node.var_node)
        self.visit(node.type_node)

    def visit_TypeSpec(self, node):
        print(type(node).__name__)
        print(node.value)

    def visit_Variable(self, node):
        print(type(node).__name__)
        print(node.value)

    def visit_Compound(self, node):
        print(type(node).__name__)
        for statement in node.statement_list:
            self.visit(statement)

    def visit_Assign(self, node):
        print(type(node).__name__)
        self.visit(node.left)
        self.visit(node.right)

    def visit_BinOp(self, node):
        print(type(node).__name__)
        self.visit(node.left)
        print(node.op.type)
        self.visit(node.right)

    def visit_UnaryOp(self, node):
        print(type(node).__name__)
        print(node.op.type)
        self.visit(node.factor)

    def visit_Empty(self, node):
        print(type(node).__name__)

    def visit_Num(self, node):
        print(type(node).__name__)
        print(node.value)

    def build_ast(self):
        self.visit(self.ast)






class Symbol(object):

    def __init__(self, name, type=None):
        self.type = type
        self.name = name

class BuiltinTypeSymbol(Symbol):

    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return f'<{self.__class__.__name__}(name={self.name})>'

class VarSymbol(Symbol):

    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return f'<{self.__class__.__name__}(name={self.name}, type={self.type})>'

    __repr__ = __str__

class SymbolTable(object):

    def __init__(self):
        self._symbols = {}
        self._define_builtins()

    def _define_builtins(self):
        self.insert(BuiltinTypeSymbol('INTEGER'))
        self.insert(BuiltinTypeSymbol('REAL'))

    def insert(self, symbol):
        print('Insert: ' + symbol.name)
        self._symbols[symbol.name] = symbol

    def lookup(self, name):
        print('Lookup: ' + name)
        symbol = self._symbols.get(name)
        return symbol

    def __str__(self):
        rtn_str = '\nSymbol Table Contents \n'
        rtn_str = rtn_str + '_'*len(rtn_str) + '\n'
        sym_str = ''
        #the 'value' needs to call the __repr__ function but I don't know the
        #syntax for f-strings to do that
        sym_str = sym_str.join([f'{key} {value}\n' 
                            for key,value in self._symbols.items()])

        return rtn_str + sym_str

    __repr__ = __str__


class SemanticAnalyzer(NodeVisitor):

    def __init__(self, ast):
        self.ast = ast
        self.symtab = SymbolTable()

    def visit_Program(self, node):
        self.visit(node.block)

    def visit_Procedure(self, node):
        pass
        #self.visit(node.block)

    def visit_Block(self, node):
        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_VarDecl(self, node):
        type_symbol = self.visit(node.type_node)
        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)

        if self.symtab.lookup(var_name) != None:
            raise Exception(f'Duplicate identifier found: [{var_name}]')

        self.symtab.insert(var_symbol)

    def visit_TypeSpec(self, node):
        return self.symtab.lookup(node.value)

    def visit_Variable(self, node):
        var_symbol = self.symtab.lookup(node.value)

        if var_symbol == None:
            raise Exception(f'Variable [{node.value}] not declared.')

    def visit_Compound(self, node):
        for statement in node.statement_list:
            self.visit(statement)

    def visit_Assign(self, node):
        self.visit(node.right)
        self.visit(node.left)

    def visit_BinOp(self, node):
        self.visit(node.right)
        self.visit(node.left)

    def visit_UnaryOp(self, node):
        self.visit(node.factor)

    def visit_Empty(self, node):
        pass

    def visit_Num(self, node):
        pass

    def analyze(self):
        self.visit(self.ast)





class Interpreter(NodeVisitor):
    
    GLOBAL_SCOPE = OrderedDict()

    def __init__(self, ast):
        self.ast = ast

    def visit_Program(self, node):
        self.visit(node.block)

    def visit_Procedure(self, node):
        pass

    def visit_Block(self, node):
        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_VarDecl(self, node):
        #used in symantic analysis
        pass

    def visit_TypeSpec(self, node):
        #used in symantic analysis
        pass

    def visit_Variable(self, node):
        var_name = node.value
        value = self.GLOBAL_SCOPE.get(var_name)

        if value == None:
            raise Exception(f'Variable {var_name} not assigned.')

        return value

    def visit_Compound(self, node):
        for statement in node.statement_list:
            self.visit(statement)

    def visit_Assign(self, node):
        var_name = node.left.value
        value = self.visit(node.right)

        self.GLOBAL_SCOPE[var_name] = value

    def visit_BinOp(self, node):
        left = node.left
        right = node.right
        op_type = node.op.type

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
        factor = node.factor

        if op_type == ADD:
            return +self.visit(factor)
        if op_type == SUB:
            return -self.visit(factor)

    def visit_Empty(self, node):
        pass

    def visit_Num(self, node):
        return node.value

    def interpret(self):
        self.visit(self.ast)


def main():

    text = open(sys.argv[1], 'r').read()

    lexer = Lexer(text)
    parser = Parser(lexer)
    ast = parser.parse()

    visual_ast = VisualAST(ast)
    visual_ast.build_ast()

    analyzer = SemanticAnalyzer(ast)
    analyzer.analyze()
    print(analyzer.symtab)

    interpreter = Interpreter(ast)
    interpreter.interpret()
    print(interpreter.GLOBAL_SCOPE)

if __name__ == "__main__":
    main()
