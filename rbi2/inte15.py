"""
Lesson 15

Adding better error reporting and error locating.

Using Python's Enumeration for token types and keywords. 

However, this implementation doesn't appear to me to offer a significant 
advancement over having module level constants. Maybe I'm missing something. 
It seems more complicated than necessary and doesn't save that much work
adding new keywords in one place instead of two. 
"""

import sys
from collections import namedtuple
from string import whitespace
from enum import Enum
import argparse

"""
ADD         = 'ADD'
SUB         = 'SUB'
MUL         = 'MUL'
INT_DIV     = 'INT_DIV'
REAL_DIV    = 'REAL_DIV'
OPAR        = 'OPAR'
CPAR        = 'CPAR'

REAL_CONST  = 'REAL_CONST'
INT_CONST   = 'INT_CONST'

REAL_TYPE   = 'REAL_TYPE'
INT_TYPE    = 'INT_TYPE'

DOT         = 'DOT'
COMMA       = 'COMMA'
SEMI        = 'SEMI'
COLON       = 'COLON'

PROGRAM     = 'PROGRAM'
PROCEDURE   = 'PROCEDURE'
VAR         = 'VAR'
BEGIN       = 'BEGIN'
END         = 'END'
NUM         = 'NUM'
ID          = 'ID'
EOF         = 'EOF'

Token = namedtuple('Token',['value', 'type'])

RESERVED_KEYWORDS = {
    'PROGRAM'   : Token('PROGRAM':PROGRAM),
    'PROCEDURE' : Token('PROCEDURE':PROCEDURE),
    'VAR'       : Token('VAR':VAR),
    'BEGIN'     : Token('BEGIN':BEGIN),
    'END'       : Token('END':END),
    'INTEGER'   : Token('INTEGER':INT_TYPE),
    'REAL'      : Token('REAL':REAL_TYPE)
    }
"""





class ErrorCode(Enum):
    UNEXPECTED_TOKEN    = 'Unexpected token'
    ID_NOT_FOUND        = 'Identifier not found'
    DUPLICATE_ID        = 'Duplicate id found'

class Error(Exception):
    
    def __init__(self, error_code=None, token=None, message=None):
        self.error_code = error_code
        self.token = token
        self.message = f'{self.__class__.__name__}: {message}'

class LexerError(Error):
    pass

class ParserError(Error):
    pass

class SemanticError(Error):
    pass







class TokenType(Enum):
    # single-char token types
    ADD         = '+'
    SUB         = '-'
    MUL         = '*'
    REAL_DIV    = '/'
    OPAR        = '('
    CPAR        = ')'

    DOT         = '.'
    COMMA       = ','
    SEMI        = ';'
    COLON       = ':'

    #reserved keywords
    PROGRAM     = 'PROGRAM' #marks beginnging of keyword block
    INT_TYPE    = 'INTEGER'
    REAL_TYPE   = 'REAL'
    INT_DIV     = 'INT_DIV'
    VAR         = 'VAR'
    PROCEDURE   = 'PROCEDURE'
    BEGIN       = 'BEGIN'
    END         = 'END' #marks end of keyword block

    #misc 
    ID          = 'ID'
    INT_CONST   = 'INT_CONST'
    REAL_CONST  = 'REAL_CONST'
    ASSIGN      = ':='
    EOF         = 'EOF'


def _build_reserved_keywords():
    """Build dictionary of reserved keywords..

    The function relies on the fact that in the TokenType enumeration
    the beginning of the block of keywords is marked with PROGRAM and
    the end of the block is marked with the END keyword.

    Result:
        {'PROGRAM': <TokenType.PROGRAM: 'PROGRAM'>,
         'INTEGER': <TokenType.INTEGER: 'INTEGER'>,
         'REAL':    <TokenType.REAL: 'REAL'>,
         'DIV':     <TokenType.DIV: 'DIV'>,
         'VAR':     <TokenType.VAR: 'VAR'>,
         'PROECDURE:<TokenType.PROCEDURE: 'PROCEDURE'>,
         'BEGIN': <TokenType.BEGIN: 'BEGIN'>,
         'END': <TokenType.END: 'END'>}
    """
    tt_list = list(TokenType)
    start_index = tt_list.index(TokenType.PROGRAM)
    end_index = tt_list.index(TokenType.END)
    reserved_keywords = {
        token_type.value: token_type
        for token_type in tt_list[start_index:end_index + 1]
    }
    return reserved_keywords

RESERVED_KEYWORDS = _build_reserved_keywords()





Token = namedtuple('Token',['value', 'type', 'lineno', 'col'])





class Lexer(object):

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]
        self.lineno = 1
        self.col = 1

    def error(self):
        s = (f"Lexer error on '{self.cur_char}' " +
                f"line: {self.lineno} column: {self.col}")

        raise LexerError(message=s)

    def get_next_char(self):
        if self.cur_char == '\n':
            self.lineno += 1
            self.col = 0 #set to zero to properly align

        self.pos += 1
        if self.pos < len(self.text) - 1:
            self.cur_char = self.text[self.pos]
            self.col += 1
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
            value += self.cur_char
            self.get_next_char()

    def get_comment(self):
        value = ''
        while self.cur_char != None and self.cur_char != '}':
            value += self.cur_char
            self.get_next_char()

        #get closing bracket
        value += self.cur_char
        self.get_next_char()

    def get_num(self):
        token_lineno = self.lineno
        token_col = self.col

        value = ''
        while self.cur_char != None and self.cur_char.isdigit():
            value += self.cur_char
            self.get_next_char()

        if self.cur_char == '.':
            value += self.cur_char
            self.get_next_char()

            while self.cur_char != None and self.cur_char.isdigit():
                value += self.cur_char
                self.get_next_char()
            
            token_value = float(value)
            token_type = TokenType.REAL_CONST

        else:
            token_value = int(value)
            token_type = TokenType.INT_CONST

        #rearranged where the token object is built
        #example has it build first because the class Token
        #can handle updating the attributes after object initiation

        #however using the named_tuple that doesn't work. Tuples
        #are immutable so updating the attributes is not possible
        #after initiation. Finally, if the code is slightly reorganized
        #it still works with named tuples
        token = Token(
                value=token_value, 
                type=token_type, 
                lineno=token_lineno, 
                col=token_col)

        return token

    def get_id(self):
        token_lineno = self.lineno
        token_col = self.col

        value = ''
        while (self.cur_char != None and 
            (self.cur_char.isalnum() or self.cur_char == '_')):
            value += self.cur_char
            self.get_next_char()

        value = value.upper()
        
        token_type = RESERVED_KEYWORDS.get(value)
        if token_type == None:
            token_type = TokenType.ID
            token_value = value
        else:
            token_type = token_type
            token_value = value

        token = Token(
                value=token_value, 
                type=token_type, 
                lineno=token_lineno, 
                col=token_col)

        return token

    def get_next_token(self):
        

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
                token = Token(
                    value=TokenType.ASSIGN.value, 
                    type=TokenType.ASSIGN, 
                    lineno=self.lineno, 
                    col=self.col)

                self.get_next_char()
                self.get_next_char()
                return token
            try:
                token_type = TokenType(self.cur_char)
            except:
                self.error()
            else:
                token = Token(
                    value=token_type.value,
                    type=token_type,
                    lineno=self.lineno,
                    col=self.col)

                self.get_next_char()
                return token 

        return Token(None, TokenType.EOF, self.lineno, self.col)






class AST(object):
    pass

class Program(AST):

    def __init__(self, name, block):
        self.name = name
        self.block = block

class Procedure(AST):

    def __init__(self, name, fpl, block):
        self.name = name
        self.fpl = fpl
        self.block = block

class Block(AST):

    def __init__(self, declarations, compound_statements):
        self.declarations = declarations

        self.compound_statements = compound_statements

class VarDecl(AST):

    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

class TypeSpec(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value

class ParamDecl(AST):

    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

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

    def __init__(self, left, factor):
        self.left = self.op = left
        self.factor = factor

class Variable(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value

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

    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)

    def error(self, error_code, token):
        raise ParserError(
            error_code = error_code,
            token = token,
            message = f'{error_code.value} -> {token}'
        )

    def check_token_type(self, token_type):
        self.log(self.cur_token)
        if self.cur_token.type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error(ErrorCode.UNEXPECTED_TOKEN, self.cur_token)

    def program(self):
        """program: PROGRAM ID SEMI block DOT"""

        self.check_token_type(TokenType.PROGRAM)
        name = self.cur_token.value
        self.check_token_type(TokenType.ID)
        self.check_token_type(TokenType.SEMI)

        block = self.block()

        self.check_token_type(TokenType.DOT)

        return Program(name, block)
        

    def block(self):
        """block: declarations compound_statements"""

        declarations = self.declarations()

        compound_statements = self.compound_statements()

        return Block(declarations, compound_statements)

    def declarations(self):
        """declarations: VAR (var_decl SEMI)+ |
                         proc_decl |
                         empty
        """
        decls = []
        if self.cur_token.type == TokenType.VAR:
            self.check_token_type(TokenType.VAR)
            while self.cur_token.type == TokenType.ID:
                decls.extend(self.var_decl())
                self.check_token_type(TokenType.SEMI)

        while self.cur_token.type == TokenType.PROCEDURE:
            decls.append(self.proc_decl())

        return decls 

    def compound_statements(self):
        """compound_statements: BEGIN statement_list END"""
        self.check_token_type(TokenType.BEGIN)
        statements = self.statement_list()
        self.check_token_type(TokenType.END)

        compound = Compound()
        for statement in statements:
            compound.statements.append(statement)

        return compound

    def statement_list(self):
        """statement_list: statement |
                           statement SEMI statement_list
        """
        statements = [self.statement()]

        while self.cur_token.type == TokenType.SEMI:
            self.check_token_type(TokenType.SEMI)
            statements.append(self.statement())

        #the last statement after the SEMI is read by the loop above
        #if there is not another SEMI that is end of the statment_list
        #and the end of the compound_statement. Therefore, there cannot
        #be another ID indicating another assignment_statement

        #this check ensures the above
        if self.cur_token.type == TokenType.ID:
            self.error(ErrorCode.UNEXPECTED_TOKEN, self.cur_token)

        return statements

    def statement(self):
        """statement: compound_statement |
                      assign_statement |
                      empty
        """

        if self.cur_token.type == TokenType.BEGIN:
            return self.compound_statements()
        if self.cur_token.type == TokenType.ID:
            return self.assign_statement()
        else:
            return self.empty()

    def assign_statement(self):
        """assign_statement: ID ASSIGN expr1"""
        left = self.variable()

        op = self.cur_token
        self.check_token_type(TokenType.ASSIGN)

        right = self.expr1()

        return Assign(left, op, right)

    def empty(self):
        """empty: """
        return Empty()

    def var_decl(self):
        """var_decl: ID (COMMA ID)* COLON type_spec"""
        var_nodes = [self.variable()]

        while self.cur_token.type == TokenType.COMMA:
            self.check_token_type(TokenType.COMMA)
            var_nodes.append(self.variable())

        self.check_token_type(TokenType.COLON)

        type_node = self.type_spec()

        var_decls = []
        for var_node in var_nodes:
            var_decls.append(VarDecl(var_node, type_node))

        return var_decls

    def type_spec(self):
        """type_spec: INT_TYPE | REAL_TYPE"""
        token = self.cur_token

        if self.cur_token.type == TokenType.INT_TYPE:
            self.check_token_type(TokenType.INT_TYPE)
        if self.cur_token.type == TokenType.REAL_TYPE:
            self.check_token_type(TokenType.REAL_TYPE)

        return TypeSpec(token)

    def proc_decl(self):
        """procedure: PROCEDURE ID (OPAR fpl CPAR)? SEMI block SEMI|
                      empty
        """
        self.check_token_type(TokenType.PROCEDURE)
        name = self.cur_token.value
        self.check_token_type(TokenType.ID)

        if self.cur_token.type == TokenType.OPAR:
            self.check_token_type(TokenType.OPAR)
            fpl = self.formal_param_list()
            self.check_token_type(TokenType.CPAR)

        self.check_token_type(TokenType.SEMI)

        block = self.block()

        self.check_token_type(TokenType.SEMI)

        return Procedure(name, fpl, block)

    def formal_param_list(self):
        """formal_parameter_list: formal_param |
                                  formal_param SEMI formal_parameter_list
        """
        #formal_param() returns a list
        #don't cast return value as a list otherwise
        #there is a list inside another list which is not
        #the datastructure wanted here.
        fpl = self.formal_param()

        while self.cur_token.type == TokenType.SEMI:
            self.check_token_type(TokenType.SEMI)
            fpl.extend(self.formal_param())

        return fpl

    def formal_param(self):
        """formal_parameter: ID (COMMA ID)* COLON type_spec"""
        var_nodes = [self.variable()]

        while self.cur_token.type == TokenType.COMMA:
            self.check_token_type(TokenType.COMMA)
            var_nodes.append(self.variable())

        self.check_token_type(TokenType.COLON)

        type_node = self.type_spec()

        fps = []
        for var_node in var_nodes:
            fps.append(ParamDecl(var_node, type_node))

        return fps
        
    def expr1(self):
        """expr1: expr2 ((ADD|SUB) expr2)*"""
        node = self.expr2()

        while self.cur_token.type in (TokenType.ADD, TokenType.SUB):
            token = self.cur_token
            if token.type == TokenType.ADD:
                self.check_token_type(TokenType.ADD)
            if token.type == TokenType.SUB:
                self.check_token_type(TokenType.SUB)

            node = BinOp(node, token, self.expr2())

        return node

    def expr2(self):
        """expr2: expr3 ((MUL|INT_DIV|FLOAT_DIV) expr3)*"""
        node = self.expr3()

        while self.cur_token.type in (TokenType.MUL, TokenType.INT_DIV, TokenType.REAL_DIV):
            token = self.cur_token
            if token.type == TokenType.MUL:
                self.check_token_type(TokenType.MUL)
            if token.type == TokenType.INT_DIV:
                self.check_token_type(TokenType.INT_DIV)
            if token.type == TokenType.INT_REAL:
                self.check_token_type(TokenType.INT_REAL)

            node = BinOp(node, token, self.expr2())

        return node

    def expr3(self):
        """expr3: ADD expr3 |
                  SUB expr3 |
                  OPAR expr1 CPAR |
                  variable |
                  num
        """

        token = self.cur_token

        if token.type == TokenType.ADD:
            self.check_token_type(TokenType.ADD)
            return UnaryOp(token, self.expr3)
        if token.type == TokenType.SUB:
            self.check_token_type(TokenType.SUB)
            return UnaryOp(token, self.expr3)
        if token.type == TokenType.OPAR:
            self.check_token_type(TokenType.OPAR)
            return self.expr1()
            self.check_token_type(TokenType.CPAR)
        if token.type == TokenType.ID:
            return self.variable()
        if token.type in (TokenType.REAL_CONST, TokenType.INT_CONST):
            return self.num()

    def variable(self):
        """variable: ID"""

        token = self.cur_token
        self.check_token_type(TokenType.ID)
        return Variable(token)

    def num(self):
        """num: REAL_CONST | INT_CONST"""

        token = self.cur_token

        if token.type == TokenType.REAL_CONST:
            self.check_token_type(TokenType.REAL_CONST)
        if token.type == TokenType.INT_CONST:
            self.check_token_type(TokenType.INT_CONST)

        return Num(token)

    def parse(self):
        root = self.program()
        if root == None:
            return None
        return root







class NodeVisitor(object):

    def visit(self, node):
        visitor_name = f'visit_{type(node).__name__}'
        visitor = getattr(self, visitor_name, self.visitor_default)
        return visitor(node)

    def visitor_default(self, node):
        raise Exception(f'Method name visit_{type(node).__name__} not found.')



class VisualAST(NodeVisitor):

    def __init__(self, root):
        self.root = root
        
    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)

    def visit_Program(self, node):
        self.log(f'{type(node).__name__}')
        self.log(node.name)

        self.visit(node.block)

    def visit_Block(self, node):
        self.log(f'{type(node).__name__}')

        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_Procedure(self, node):
        self.log(f'{type(node).__name__}')
        self.log(node.name)

        for fp in node.fpl:
            self.visit(fp)

        self.visit(node.block)

    def visit_VarDecl(self, node):
        self.log(f'{type(node).__name__}')

        self.visit(node.var_node)

        self.visit(node.type_node)

    def visit_TypeSpec(self, node):
        self.log(f'{type(node).__name__}')
        
        self.log(node.value)

    def visit_Compound(self, node):
        self.log(f'{type(node).__name__}')

        for statement in node.statements:
            self.visit(statement)

    def visit_ParamDecl(self, node):
        self.log(f'{type(node).__name__}')

        self.visit(node.var_node)

        self.visit(node.type_node)

    def visit_Assign(self, node):
        self.log(f'{type(node).__name__}')

        self.visit(node.left)

        self.log(node.op.value)

        self.visit(node.right)

    def visit_BinOp(self, node):
        self.log(f'{type(node).__name__}')

        self.visit(node.left)

        self.log(node.op.value)

        self.visit(node.right)

    def visit_UnaryOp(self, node):
        self.log(f'{type(node).__name__}')

        self.visit(node.left.value)

        self.visit(node.factor)

    def visit_Variable(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.value)

    def visit_Num(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.value)

    def visit_Empty(self, node):
        self.log(f'{type(node).__name__}')

    def build_ast(self):
        self.visit(self.root)







class Symbol(object):

    def __init__(self, name, scope, type=None):
        self.name = name
        self.type = type

class BuiltinSymbolType(Symbol):

    def __init__(self, name, scope):
        super().__init__(name, scope)

    def __str__(self):
        return self.name

    def __repr__(self):
        return f'{self.__class__.__name__}<(name={self.name})>'

class VarSymbol(Symbol):

    def __init__(self, name, scope, type):
        super().__init__(name, scope, type)

    def __str__(self):
        return f'{self.__class__.__name__}<(name={self.name}, type={self.type})>'

    __repr__ = __str__

class ProcSymbol(Symbol):

    def __init__(self, name, scope, params = None):
        super().__init__(name, scope)
        self.params = params if params != None else []

    def __str__(self):
        return f'{self.__class__.__name__}<(name={self.name}, params={self.params})>'

    __repr__ = __str__

class ProgSymbol(Symbol):

    def __init__(self, name, scope):
        super().__init__(name, scope)

    def __str__(self):
        return f'{self.__class__.__name__}<(name={self.name})>'

    __repr__ = __str__
    
class ScopedSymbolTable(object):

    def __init__(self, name, level, enclosing_scope):
        self._symbols = {}
        self.name = name
        self.level = level
        self.enclosing_scope = enclosing_scope

    def _init_builtins(self, scope):
        self.insert(BuiltinSymbolType('INTEGER', scope))
        self.insert(BuiltinSymbolType('REAL', scope))

    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)

    def insert(self, symbol):
        self.log(f'INSERT: {symbol.name}')
        self._symbols[symbol.name] = symbol

    def lookup(self, name, cur_scope_only = False):
        self.log(f'LOOKUP: {name} (Scope name: {self.name})')
        symbol = self._symbols.get(name)

        #if symbol is found in current scope, return symbol
        if symbol != None:
            return symbol

        #if only searching current scope
        #and symbol is not found in current scope (handled by above if-statement)
        #return None
        if cur_scope_only:
            return None

        #if searching all scopes
        #and symbol is not round in current scope
        #recursively search up the enclosing scopes
        if self.enclosing_scope != None:
            return self.enclosing_scope.lookup(name)  

    def __str__(self):
        lines = []
        line = 'SCOPE (SCOPED SYMBOL TABLE)'
        line2 = '=' * len(line)
        lines.append(line)
        lines.append(line2)

        data = {'Scope name':self.name,
                'Scope level':self.level}

        for key, val in data.items():
            lines.append(f'{key:15}: {val}')

        line3 = 'Scope (Scoped symbol table) contents'
        line4 = '-' * len(line3)
        lines.append(line3)
        lines.append(line4)

        for key, val in self._symbols.items():
            lines.append(f'{key:7}: {val}')

        scope_str = '\n'.join(lines)

        return scope_str

    __repr__ = __str__







class SemanticAnalyzer(NodeVisitor):

    def __init__(self, root):
        self.root = root
        self.cur_scope = None

    def _init_outer_scope(self):
        outer_scope = ScopedSymbolTable(
                    name = 'outer',
                    level = 0,
                    enclosing_scope = self.cur_scope)

        outer_scope._init_builtins(outer_scope)

        return outer_scope

    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)

    def error(self, error_code, token):
        raise SemanticError(
            error_code = error_code,
            token = token,
            message = f'{error_code.value} -> {token}')

    def visit_Program(self, node):
        prog_symbol = ProgSymbol(node.name, self.cur_scope)
        self.cur_scope.insert(prog_symbol)

        global_scope = ScopedSymbolTable(
                    name = 'global',
                    level = 1,
                    enclosing_scope = self.cur_scope)

        self.log('\nENTERING: global scope')
        self.cur_scope = global_scope

        self.visit(node.block)

        self.log(f'\nLEAVING: {self.cur_scope.name}')
        self.log(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope

    def visit_Procedure(self, node):
        proc_symbol = ProcSymbol(node.name, self.cur_scope)
        self.cur_scope.insert(proc_symbol)

        proc_scope = ScopedSymbolTable(
                    name = node.name,
                    level = self.cur_scope.level + 1,
                    enclosing_scope = self.cur_scope)

        self.log(f'\nENTERING: {proc_scope.name}')
        self.cur_scope = proc_scope

        params = []
        for fp in node.fpl:
            params.append(self.visit(fp))

        proc_symbol.params = params

        self.visit(node.block)

        self.log(f'\nLEAVING: {self.cur_scope.name}')
        self.log(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope
        

    def visit_Block(self, node):

        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_VarDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.cur_scope.lookup(type_name)
        
        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, self.cur_scope, type_symbol)

        if self.cur_scope.lookup(var_name, cur_scope_only=True) != None:
            self.error(ErrorCode.DUPLICATE_ID, node.var_node.token)

        self.cur_scope.insert(var_symbol)

    def visit_TypeSpec(self, node):
        pass

    def visit_Compound(self, node):

        for statement in node.statements:
            self.visit(statement)

    def visit_ParamDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.cur_scope.lookup(type_name)
        
        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, self.cur_scope, type_symbol)

        if self.cur_scope.lookup(var_name, cur_scope_only=True) != None:
            self.error(ErrorCode.DUPLICATE_ID, node.var_node.token)

        self.cur_scope.insert(var_symbol)

        return var_symbol

    def visit_Assign(self, node):
        self.visit(node.right)
        self.visit(node.left)

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_UnaryOp(self, node):
        self.visit(node.left)

    def visit_Variable(self, node):
        var_symbol = self.cur_scope.lookup(node.value)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)

        #no return, method simply checks that the symbol is in the
        #scoped symbol table; if not an error is raised

    def visit_Num(self, node):
        pass

    def visit_Empty(self, node):
        pass

    def analyze(self):
        self.log('\n\nENTERING: outer_scope')
        outer_scope = self._init_outer_scope()
        self.cur_scope = outer_scope

        self.visit(self.root)

        self.log(f'\nLEAVING: {self.cur_scope.name}')
        self.log(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope





'''
    def visit_Program(self, node):

    def visit_Procedure(self, node):

    def visit_Block(self, node):

    def visit_VarDecl(self, node):

    def visit_TypeSpec(self, node):

    def visit_Compound(self, node):

    def visit_ParamDecl(self, node):

    def visit_Assign(self, node):

    def visit_BinOp(self, node):

    def visit_UnaryOp(self, node):

    def visit_Variable(self, node):

    def visit_Num(self, node):

    def visit_Empty(self, node):
'''







def main():

    parser = argparse.ArgumentParser(
        description='Pascal Interpreter')

    parser.add_argument('inputfile', help='Pascal source file')
    parser.add_argument(
            '--scope',
            help='Print scope information',
            action='store_true',)

    args = parser.parse_args()

    global _SHOULD_LOG_SCOPE    
    _SHOULD_LOG_SCOPE = args.scope

    text = open(args.inputfile, 'r').read()

    lexer = Lexer(text)
#    try:
#       cur_token = lexer.get_next_token()
#       while cur_token.type != TokenType.EOF:
#           print(cur_token)
#           cur_token = lexer.get_next_token()
#    except LexerError as e:
#        print(e.message)

    try:
        parser = Parser(lexer)
        root = parser.parse()
    except (LexerError, ParserError) as e:
        print(e.message)
        sys.exit(1)

    visual_ast = VisualAST(root)
    visual_ast.build_ast()

    try:
        analyzer = SemanticAnalyzer(root)
        analyzer.analyze()
    except SemanticError as e:
        print(e.message)
        sys.exit(1)

if __name__ == '__main__':
    main()

    print(_SHOULD_LOG_SCOPE)
