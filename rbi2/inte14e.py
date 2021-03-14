"""
Lesson 14 - Part 5

I wanted to write the whole thing from scratch again.

I like the successive build up one part at a time for the initial learning
but I like to go back and stick it all together at once. 

Plus repition is good for learning.

+ - one or more times
* - zero or more times
? - zero or one times

"""

from collections import namedtuple
import pdb
from string import whitespace

ADD         = 'ADD'
SUB         = 'SUB'
MUL         = 'MUL'
INT_DIV     = 'INT_DIV'
REAL_DIV    = 'REAL_DIV'
OPAR        = 'OPAR'
CPAR        = 'CPAR'
NUM         = 'NUM'

DOT         = 'DOT'
COMMA       = 'COMMA'
SEMI        = 'SEMI'
COLON       = 'COLON'

INT_CONST   = 'INT_CONST'
REAL_CONST  = 'REAL_CONST'

INT_TYPE    = 'INT_TYPE'
REAL_TYPE   = 'REAL_TYPE'

PROGRAM     = 'PROGRAM'
PROCEDURE   = 'PROCEDURE'
BEGIN       = 'BEGIN'
END         = 'END'
VAR         = 'VAR'
ID          = 'ID'
ASSIGN      = 'ASSIGN'
EOF         = 'EOF'

Token = namedtuple('Token', ['value', 'type'])

RESERVED_KEYWORDS = {
    'PROGRAM'   : Token('PROGRAM', PROGRAM),
    'PROCEDURE' : Token('PROCEDURE', PROCEDURE),
    'BEGIN'     : Token('BEGIN', BEGIN),
    'END'       : Token('END', END),
    'VAR'       : Token('VAR', VAR),
    'DIV'       : Token('DIV', INT_DIV),
    'INTEGER'   : Token('INTEGER', INT_TYPE),
    'REAL'      : Token('REAL', REAL_TYPE),
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
        if self.pos < len(self.text) - 1:
            self.cur_char = self.text[self.pos]
        else:
            self.cur_char = None

    def peek_next_char(self):
        peek_pos = self.pos + 1
        if peek_pos < len(self.text) - 1:
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

        #get closing bracket '}'
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
                    value = value + self.cur_char
                    self.get_next_char()

                return Token(value, REAL_CONST)

        return Token(value, INT_CONST)

    def get_id(self):
        value = ''
        while (self.cur_char != None and 
                (self.cur_char.isalnum() or self.cur_char == '_')):

            value = value + self.cur_char
            self.get_next_char()

        #Pascal is not case-sensitive
        value = value.upper()

        return RESERVED_KEYWORDS.get(value, Token(value, ID))

    def get_next_token(self):
        symbol_dict = {
            '+' : Token('+', ADD),
            '-' : Token('-', SUB),
            '*' : Token('*', MUL),
            '/' : Token('/', REAL_DIV),
            '.' : Token('.', DOT),
            ',' : Token(',', COMMA),
            ';' : Token(';', SEMI),
            ':' : Token(':', COLON),
            '(' : Token('(', OPAR),
            ')' : Token(')', CPAR),
        }

        while self.cur_char != None:
            if self.cur_char in whitespace:
                self.get_whitespace()
                continue
            if self.cur_char == '{':
                self.get_comment()
                continue
            if self.cur_char.isdigit():
                return self.get_num()
            if self.cur_char.isalpha() or self.cur_char == '_':
                return self.get_id()
            if self.cur_char == ':' and self.peek_next_char() == '=':
                self.get_next_char()
                self.get_next_char()
                return Token(':=', ASSIGN)
            if self.cur_char in list(symbol_dict):
                value = self.cur_char
                self.get_next_char()
                return symbol_dict[value]

            self.error()

        return Token(None, EOF)







class AST(object):
    pass

class Program(AST):

    def __init__(self, name_var, block):
        self.name_var = name_var
        self.block = block

class Block(AST):

    def __init__(self, declarations, compound_statements):
        self.declarations = declarations
        self.compound_statements = compound_statements

class Compound(AST):

    def __init__(self):
        self.statement_list = []

class ProcDecl(AST):

    def __init__(self, name_var, fpl, block):
        self.name_var = name_var
        self.fpl = fpl
        self.block = block

class Param(AST):

    def __init__(self, name, type):
        self.name = name
        self.type = type

class VarDecl(AST):

    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

class TypeSpec(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value

class Variable(AST):

    def __init__(self, token):
        self.token = token
        self.name = self.token.value

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

    def __init__(self, token, factor):
        self.token = self.left = token
        self.factor = factor

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
        name_var = self.variable()
        self.check_token_type(SEMI)

        block = self.block()
        
        self.check_token_type(DOT)

        return Program(name_var, block)

    def block(self):
        """block: declarations compound_statements"""
        declarations = self.declaration()

        compound_statements = self.compound_statement()

        return Block(declarations, compound_statements)

    def declaration(self):
        """declarations: VAR (var_decl SEMI)+ |
                         (PROCEDURE variable (OPAR formal_param_list CPAR)? SEMI block SEMI)* |
                         empty
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
                name_var = self.variable()

                if self.cur_token.type == OPAR:
                    self.check_token_type(OPAR)
                    fpl = self.formal_param_list()
                    self.check_token_type(CPAR)

                self.check_token_type(SEMI)

                block = self.block()

                self.check_token_type(SEMI)

                decls.append(ProcDecl(name_var, fpl, block))
            else:
                break

        return decls
        

    def formal_param_list(self):
        """formal_param_list: formal_param |
                              formal_param SEMI formal_param_list
        """

        fpl = self.formal_param()

        while self.cur_token.type == SEMI:
            self.check_token_type(SEMI)
            fpl.extend(self.formal_param())

        return fpl
            
    def formal_param(self):
        """formal_param: variable (COMMA variable)* COLON type_spec"""

        var_nodes = [self.variable()]

        while self.cur_token.type == COMMA:
            self.check_token_type(COMMA)
            var_names.append(self.variable())

        self.check_token_type(COLON)

        type_node = self.type_spec()

        fps = []
        for var_node in var_nodes:
            fps.append(VarDecl(var_node, type_node))

        return fps

    def var_decl(self):
        """var_decl: variable (COMMA variable)* COLON type_spec"""
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
        """type_spec: INT_TYPE | REAL_TYPE"""

        token = self.cur_token

        if token.type == INT_TYPE:
            self.check_token_type(INT_TYPE)
        if token.type == REAL_TYPE:
            self.check_token_type(REAL_TYPE)

        return TypeSpec(token)

    def variable(self):
        """variable: ID"""

        token = self.cur_token
        self.check_token_type(ID)

        return Variable(token)

    def compound_statement(self):
        """compound_statment: BEGIN statement_list END"""

        self.check_token_type(BEGIN)
        statement_list = self.statement_list()
        self.check_token_type(END)

        #I still don't understand why this is necessary
        #it seems like the statement list could just be 
        #passed to a Compound node instance. It seems
        #unnecessary to read it into a pre-defined list
        #one item at a time.
        compound = Compound()
        for statement in statement_list:
            compound.statement_list.append(statement)

        return compound

    def statement_list(self):
        """statement_list: statement |
                           statement SEMI statement_list
        """

        statement_list = [self.statement()]

        while self.cur_token.type == SEMI:
            self.check_token_type(SEMI)
            statement_list.append(self.statement())

        return statement_list

    def statement(self):
        """statement: compound_statement |
                      assign_statement |
                      empty
        """

        if self.cur_token.type == BEGIN:
            return self.compound_statement()
        if self.cur_token.type == ID:
            return self.assign_statement()
        else:
            return self.empty()

    def assign_statement(self):
        """assign_statement: variable ASSIGN expr3"""

        left = self.variable()

        token = self.cur_token
        self.check_token_type(ASSIGN)

        right = self.expr1()

        return Assign(left, token, right)

    def empty(self):
        """empty: """
        return Empty()

    def expr1(self):
        """expr1: expr2 ((ADD|SUB) expr2)*"""

        node = self.expr2()

        while self.cur_token != None and self.cur_token.type in (ADD, SUB):
            token = self.cur_token
            if self.cur_token.type == ADD:
                self.check_token_type(ADD)
            if self.cur_token.type == SUB:
                self.check_token_type(SUB)

            node = BinOp(node, token, self.expr2())

        return node

    def expr2(self):
        """expr2: expr3 ((MUL|INT_DIV|REAL_DIV) expr3)*"""

        node = self.expr3()

        while self.cur_token != None and self.cur_token.type in (MUL, INT_DIV, REAL_DIV):
            token = self.cur_token
            if self.cur_token.type == MUL:
                self.check_token_type(MUL)
            if self.cur_token.type == INT_DIV:
                self.check_token_type(INT_DIV)
            if self.cur_token.type == REAL_DIV:
                self.check_token_type(REAL_DIV)

            node = BinOp(node, token, self.expr3())

        return node

    def expr3(self):
        """expr3: ADD expr3 |
                  SUB expr3 |
                  OPAR epxr1 CPAR |
                  num |
                  variable
        """

        token = self.cur_token

        if token.type == ADD:
            self.check_token_type(ADD)
            return UnaryOp(token, self.expr3())
        if token.type == SUB:
            self.check_token_type(SUB)
            return UnaryOp(token, self.expr3())
        if token.type == OPAR:
            self.check_token_type(OPAR)
            return self.expr1()
            self.check_token_type(CPAR)
        if token.type == NUM:
            self.check_token_type(NUM)
            return self.num(token)
        if token.type == ID:
            return self.variable()

    def num(self):
        """num: REAL_CONST | INT_CONST"""

        token = self.cur_token

        if self.cur_token.type == REAL_CONST:
            self.check_token_type(REAL_CONST)
        if self.cur_token.type == INT_CONST:
            self.check_token_type(INT_CONST)

        return Num(token)

    def parse(self):
        groot = self.program()
        if groot == None:
            return None
        return groot








class NodeVisitor(object):

    def visit(self, node):
        visitor_name = 'visit_' + type(node).__name__
        visitor = getattr(self, visitor_name, self.visitor_default)
        return visitor(node)

    def visitor_default(self, node):
        raise Exception(f'Class has no method visit_{type(node).__name__}')







class VisualAST(NodeVisitor):


    def __init__(self, root):
        self.root = root

    def visit_Program(self, node):
        print(type(node).__name__)
        self.visit(node.name_var)
        self.visit(node.block)
    
    def visit_Block(self, node):
        print(type(node).__name__)
        
        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_Compound(self, node):
        print(type(node).__name__)

        for statement in node.statement_list:
            self.visit(statement)

    def visit_ProcDecl(self, node):
        print(type(node).__name__)

        self.visit(node.name_var)

        for fp in node.fpl:
            self.visit(fp)

        self.visit(node.block)

    def visit_Param(self, node):
        print(type(node).__name__)

        self.visit(var_node)
        self.visit(type_node)

    def visit_VarDecl(self, node):
        print(type(node).__name__)

        self.visit(node.var_node)
        self.visit(node.type_node)

    def visit_TypeSpec(self, node):
        print(type(node).__name__)

        print(node.value)

    def visit_Variable(self, node):
        print(type(node).__name__) 
        print(node.name)

    def visit_Assign(self, node):
        print(type(node).__name__)

        self.visit(node.left)

        print(node.token.value)

        self.visit(node.right)

    def visit_BinOp(self, node):
        print(type(node).__name__)

        self.visit(node.left)

        print(node.token.value)

        self.visit(node.right)

    def visit_UnaryOp(self, node):
        print(type(node).__name__)

        print(node.token.value)

        self.visit(node.right)

    def visit_Num(self, node):
        print(type(node).__name__)

        print(node.value)

    def visit_Empty(self, node):
        pass

    def build_ast(self):
        self.visit(self.root)






class Symbol(object):

    def __init__(self, name, type=None):
        self.name = name
        self.type = type

class BuiltinSymbolType(Symbol):

    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return f'<{self.__class__.__name__}(name={self.name})>'

class VarSymbol(Symbol):

    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return f'<{self.__class__.__name__}(name={self.name},type={self.type})>'

class ProcSymbol(Symbol):

    def __init__(self, name, params=None):
        super().__init__(name)
        self.params = params if params != None else []

    def __str__(self):
        return f'<{self.__class__.__name__}(name={self.name},params={self.params})>'

class ProgSymbol(Symbol):

    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return f'<{self.__class__.__name__}(name={self.name})>'



class ScopeSymbolTable(object):

    def __init__(self, scope_name, scope_level, enclosing_scope):
        self._symbols = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope

    def _init_builtins(self):
        self.insert(BuiltinSymbolType('INTEGER'))
        self.insert(BuiltinSymbolType('REAL'))

    def __str__(self):
        lines = []
        line1 = '\n\nSCOPE (SCOPED SYMBOL TABLE)'
        line2 = '='*len(line1)

        lines.append(line1)
        lines.append(line2)

        l1 = ['Scope name', 'Scope level', 'Enclosing scope']
        #this one got me
        #program was printing scopes nested within enclosing scopes
        #was inadverently calling the entire enclosing scope to print
        #instead of just the name
        esn = self.enclosing_scope.scope_name if self.enclosing_scope != None else None
        l2 = [self.scope_name, self.scope_level, esn ]

        for a, b in zip(l1, l2):
            ln = f'{a:15} : {b}'
            lines.append(ln)

        line3 = 'Scope (Scope symbol table) contents'
        line4 = '-'*len(line3)
        lines.append(line3)
        lines.append(line4)

        for key,value in self._symbols.items():
            ln = f'{key:7} : {value}'
            lines.append(ln)

        output = '\n'.join(lines) + '\n'

        return output 

    def insert(self, symbol):
        self._symbols[symbol.name] = symbol
        
    def lookup(self, name, cur_scope_only=False):
        symbol = self._symbols.get(name)

        if symbol != None:
            return symbol

        if cur_scope_only:
            return None 

        if self.enclosing_scope != None:
            symbol = self.enclosing_scope.lookup(name)







class SemanticAnalyzer(NodeVisitor):

    def __init__(self, root):
        self.root = root
        self.cur_scope = None

    def _init_outer_scope(self):
        print('ENTERING scope: outer')

        outer_scope = ScopeSymbolTable(
            scope_name = 'outer',
            scope_level = 0,
            enclosing_scope = None)

        outer_scope._init_builtins()

        self.cur_scope = outer_scope

    def visit_Program(self, node):
        name = node.name_var.name
        progsym = ProgSymbol(name)
        self.cur_scope.insert(progsym)

        global_scope = ScopeSymbolTable(
            scope_name = 'global',
            scope_level = self.cur_scope.scope_level + 1,
            enclosing_scope = self.cur_scope)
       
        print(f'ENTERING scope: global') 
        self.cur_scope = global_scope

        self.visit(node.block)

        print(global_scope)

        print(f'LEAVING scope: global')
        self.cur_scope = self.cur_scope.enclosing_scope
    
    def visit_Block(self, node):
        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_Compound(self, node):
        for statement in node.statement_list:
            self.visit(statement)

    def visit_ProcDecl(self, node):
        name = node.name_var.name
        procsym = ProcSymbol(name)
        self.cur_scope.insert(procsym)

        proc_scope = ScopeSymbolTable(
            scope_name = name,
            scope_level = self.cur_scope.scope_level + 1,
            enclosing_scope = self.cur_scope)

        print(f'ENTERING scope: {name}')
        self.cur_scope = proc_scope

        for param in node.fpl:
            var_symbol = self.visit(param)
            procsym.params.append(var_symbol)
            
        self.visit(node.block)

        print(proc_scope)

        print(f'LEAVING scope: {name}')
        self.cur_scope = self.cur_scope.enclosing_scope

    def visit_Param(self, node):
        type_name = node.type_node.value
        type_symbol = self.cur_scope.lookup(type_name)

        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)
        self.cur_scope.define(var_symbol)
        return var_symbol

    def visit_VarDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.cur_scope.lookup(type_name)

        var_name = node.var_node.name
        self.cur_scope.insert(VarSymbol(var_name, type_symbol))

    def visit_TypeSpec(self, node):
        pass

    def visit_Variable(self, node):
        var_name = node.name
        self.cur_scope.lookup(var_name)

    def visit_Assign(self, node):
        self.visit(node.right)

        self.visit(node.left)

    def visit_BinOp(self, node):
        self.visit(node.left)

        self.visit(node.right)

    def visit_UnaryOp(self, node):
        self.visit(node.right)

    def visit_Num(self, node):
        pass

    def visit_Empty(self, node):
        pass

    def analyze(self):
        self._init_outer_scope()
    
        self.visit(self.root)

        print(self.cur_scope)
        print(f'LEAVING scope: outer')
        self.cur_scope = self.cur_scope.enclosing_scope
        


def main():

    text = open('a14_1.txt', 'r').read()

    lexer = Lexer(text)

    #token = lexer.get_next_token()

    #while token.type != EOF:
    #    print(token)
    #    token = lexer.get_next_token()

    parser = Parser(lexer)
    root = parser.parse()

    va = VisualAST(root)
    va.build_ast()

    sa = SemanticAnalyzer(root)
    sa.analyze()

if __name__ == "__main__":
    main()
