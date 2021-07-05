"""
Lesson 19 - Nested Procedure Calls

"""

from collections import namedtuple
from enum import Enum
from string import whitespace
import sys
import pdb

pdb.disable()

class ErrorCode(Enum):

    ID_NOT_FOUND        = 'ID NOT FOUND'
    UNEXPECTED_TOKEN    = 'UNEXPECTED TOKEN'
    DUPLICATE_ID        = 'DUPLICATE ID'
    NUM_PARAMS          = 'UNEQUAL NUMBER OF PARAMS'

class Error(Exception):

    def __init__(self, error_code=None, token=None, msg=None):
        self.error_code = error_code
        self.token = token
        self.msg = msg

    def __str__(self):
        return f'{self.__class__.__name__}: {self.error_code}, msg={self.msg}'

class LexerError(Error):
    pass

class ParserError(Error):
    pass

class SemanticError(Error):
    pass

class InterpreterError(Error):
    pass



class TokenType(Enum):

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

    PROGRAM     = 'PROGRAM'
    PROCEDURE   = 'PROCEDURE'
    VAR         = 'VAR'
    INT_TYPE    = 'INTEGER'
    REAL_TYPE   = 'REAL'
    INT_DIV     = 'DIV'
    BEGIN       = 'BEGIN'
    END         = 'END'

    INT_CONST   = 'INT_CONST'
    REAL_CONST  = 'REAL_CONST'

    ID          = 'ID'
    NUM         = 'NUM'
    EOF         = 'EOF'
    ASSIGN      = 'ASSIGN'

def _reserved_keywords():
    tt_list = list(TokenType)
    start_index = tt_list.index(TokenType.PROGRAM)
    end_index = tt_list.index(TokenType.END)
    reserved_keywords = {}
    for item in tt_list[start_index:end_index+1]:
        reserved_keywords[item.value] = item

    return reserved_keywords

RESERVED_KEYWORDS = _reserved_keywords()

Token = namedtuple('Token',['value','type','lineno', 'col'])


class Lexer(object):

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]

        self.colno = 0
        self.lineno = 1

    def get_next_char(self):
        if self.cur_char == '\n':
            self.colno = 0
            self.lineno += 1

        self.pos += 1
        self.colno += 1
        if self.pos < len(self.text):
            self.cur_char = self.text[self.pos]
        else:
            self.cur_char = None

    def peek_next_char(self):
        peek_pos = self.pos + 1
        if peek_pos < len(self.text):
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
            value += value
            self.get_next_char()

        #gets closing bracket "}"
        value += self.cur_char
        self.get_next_char()

    def get_num(self):
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

            return Token(value, 
                         TokenType.REAL_CONST,
                         self.linno,
                         self.colno)

        return Token(value, 
                   TokenType.INT_CONST, 
                   self.lineno, 
                   self.colno)


    def get_id(self):
        value = ''
        while (self.cur_char != None and 
                (self.cur_char.isalnum() or self.cur_char == '_')):
            value += self.cur_char
            self.get_next_char()

        value = value.upper()

        token_type = RESERVED_KEYWORDS.get(value, TokenType.ID)

        return Token(value,
                     token_type,
                     self.lineno,
                     self.colno)

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
                self.get_next_char()
                self.get_next_char()
                return Token(':=', 
                             TokenType.ASSIGN, 
                             self.lineno, 
                             self.colno)
            else:
                try:
                    token_type = TokenType(self.cur_char)
                except ValueError as e:
                    msg = f'ID={self.cur_char}, lineno={self.lineno}, colno={self.colno}'
                    raise LexerError(error_code=ErrorCode.ID_NOT_FOUND, 
                                     msg=msg)

                token = Token(self.cur_char,
                              token_type,
                              self.lineno,
                              self.colno)

                self.get_next_char()

                return token

        return Token('EOF',
                     TokenType.EOF,
                     self.lineno,
                     self.colno)






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

    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node
        
class TypeSpec(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value

class ProcDecl(AST):

    def __init__(self, name, block, fpl=None):
        self.name = name
        self.block = block
        self.fpl = fpl if fpl!=None else []

class Compound(AST):

    def __init__(self):
        self.statements = []

class ProcCall(AST):

    def __init__(self, name, apl=None):
        self.name = name
        self.apl = [] if apl==None else apl
        self.proc_symbol = None #initialize to none

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
        self.left = left
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
        if _LOG_PARSER:
            print(msg)

    def check_token_type(self, token_type):
        self.log(self.cur_token)
        if self.cur_token.type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            msg = f'token={self.cur_token}'
            raise ParserError(error_code=ErrorCode.UNEXPECTED_TOKEN, 
                              token=self.cur_token, 
                              msg=msg)

    def program(self):
        """program: PROGRAM ID SEMI block DOT"""
        self.check_token_type(TokenType.PROGRAM)
        name = self.cur_token.value
        self.check_token_type(TokenType.ID)
        self.check_token_type(TokenType.SEMI)

        block = self.block()

        self.check_token_type(TokenType.DOT)
        self.check_token_type(TokenType.EOF)

        return Program(name, block)

    def block(self):
        """block: declarations compound_statement """

        declarations = self.declarations()

        compound_statements = self.compound_statement()

        return Block(declarations, compound_statements)

    def declarations(self):
        """declarations: VAR (var_decl SEMI)* |
                         proc_decl
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

    def proc_decl(self):
        """proc_decl: PROCEDURE ID (OPAR (param_decl SEMI)* CPAR)? SEMI block SEMI"""

        self.check_token_type(TokenType.PROCEDURE)
        name = self.cur_token.value
        self.check_token_type(TokenType.ID)

        if self.cur_token.type == TokenType.OPAR:
            self.check_token_type(TokenType.OPAR)

            fpl = self.param_decl()
            while self.cur_token.type == TokenType.SEMI:
                self.check_token_type(TokenType.SEMI)
                fpl.extend(self.param_decl())

            self.check_token_type(TokenType.CPAR)

        self.check_token_type(TokenType.SEMI)

        block = self.block()

        self.check_token_type(TokenType.SEMI)

        return ProcDecl(name, block, fpl)

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

    def param_decl(self):
        """param_decl: ID (COMMA ID)* COLON type_spec"""
                
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

    def compound_statement(self):
        """compound_statement: BEGIN statement_list END"""
        #pdb.set_trace()
        self.check_token_type(TokenType.BEGIN)

        statement_list = self.statement_list()

        self.check_token_type(TokenType.END)

        compound = Compound()
        for statement in statement_list:
            compound.statements.append(statement)

        return compound

    def statement_list(self):
        """statement_list: statement |
                           statement SEMI statement_list
        """
        statement_list = [self.statement()]

        while self.cur_token.type == TokenType.SEMI:
            self.check_token_type(TokenType.SEMI)
            statement_list.append(self.statement())

        #if there is an ID following the while loop
        #above it means that there is an ID token
        #that didn't match to the statement grammar
        #in the statment() function and this means
        #there is an error which is caught here
        if self.cur_token.type == TokenType.ID:
            msg = f'token={self.cur_token}'
            raise ParserError(error_code=ErrorCode.UNEXPECTED_TOKEN, 
                              token=self.cur_token, 
                              msg=msg)

        return statement_list

    def statement(self):
        """statement: compound_statement |
                      proc_call |
                      assign_statment |
                      empty_statement
        """

        token = self.cur_token

        if token.type == TokenType.BEGIN:
            return self.compound_statement()
        if token.type == TokenType.ID and self.lexer.cur_char == "(":
            return self.proc_call()
        if token.type == TokenType.ID:
            return self.assign_statement()
        else:
            return self.empty()

    def proc_call(self):
        """proc_call: ID OPAR (apl)? CPAR SEMI"""
        name = self.cur_token.value
        self.check_token_type(TokenType.ID)

        self.check_token_type(TokenType.OPAR)

        if self.cur_token.type != TokenType.CPAR:
            apl = self.actual_param_list()

        self.check_token_type(TokenType.CPAR)

        self.check_token_type(TokenType.SEMI)

        return ProcCall(name, apl)

    def actual_param_list(self):
        """actual_param_list: expr1 (COMMA expr1)*"""
        apl = [self.expr1()]

        while self.cur_token.type == TokenType.COMMA:
            self.check_token_type(TokenType.COMMA)
            apl.append(self.expr1())

        return apl

    def assign_statement(self):
        """assign_statement: variable ASSIGN expr1"""
        left = self.variable()

        token = self.cur_token
        self.check_token_type(TokenType.ASSIGN)

        right = self.expr1()

        return Assign(left, token, right)

    def empty(self):
        """empty: """
        return Empty()

    def expr1(self):
        """expr1: expr2 ((ADD|SUB) expr2)* """

        node = self.expr2()

        while self.cur_token.type in (TokenType.ADD, TokenType.SUB):
            token = self.cur_token
            if token.type == TokenType.ADD:
                self.check_token_type(TokenType.ADD)
            if token.type == TokenType.SUB:
                self.check_token_type(TokenTYpe.SUB)

            node = BinOp(left=node, token=token, right=self.expr2())

        return node

    def expr2(self):
        """expr2: expr3 ((MUL|INT_DIV|REAL_DIV) expr3)* """

        node = self.expr3()

        while (self.cur_token.type in 
                (TokenType.MUL, TokenType.INT_DIV, TokenType.REAL_DIV)):
            token = self.cur_token
            if token.type == TokenType.MUL:
                self.check_token_type(TokenType.MUL)
            if token.type == TokenType.INT_DIV:
                self.check_token_type(TokenType.INT_DIV)
            if token.type == TokenType.REAL_DIV:
                self.check_token_type(TokenType.REAL_DIV)

            node = BinOp(left=node, token=token, right=self.expr3())

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
            return UnaryOp(token, self.expr3())
        if token.type == TokenType.SUB:
            self.check_token_type(TokenType.SUB)
            return UnaryOp(token, self.expr3())
        if token.type == TokenType.OPAR:
            self.check_token_type(TokenType.OPAR)
            result = self.expr1()
            self.check_token_type(TokenType.CPAR)
            return result
        if token.type == TokenType.ID:
            return self.variable()
        if token.type in (TokenType.INT_CONST, TokenType.REAL_CONST):
            return self.num()
            
    def variable(self):
        """variable: ID"""
        token = self.cur_token
        self.check_token_type(TokenType.ID)
        return Variable(token)

    def num(self):
        """num: INT_CONST | REAL_CONST"""
        token = self.cur_token

        if token.type == TokenType.INT_CONST:
            self.check_token_type(TokenType.INT_CONST)
        if token.type == TokenType.REAL_CONST:
            self.check_token_type(TokenType.REAL_CONST)
        
        return Num(token)

    def parse(self):
        root = self.program()
        if root == None:
            return None
        return root        
    






class NodeVisitor(object):

    def visit(self, node):
        visit_name = f'visit_{type(node).__name__}'
        visit_method = getattr(self, visit_name, self.visit_default)
        return visit_method(node)

    def visit_default(self, node):
        raise Exception(f'Class method {type(node).__name__}  not found.')







class VisualAST(NodeVisitor):

    def __init__(self, root):
        self.root = root

    def log(self, msg):
        if _LOG_VISUAL:
            print(f'{msg}')

    def visit_Program(self, node):
        self.log(f'{type(node).__name__}')
        self.log(node.name)

        self.visit(node.block)

    def visit_Block(self, node):
        self.log(f'{type(node).__name__}')

        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_VarDecl(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.var_node.value)
        self.visit(node.type_node)

    def visit_TypeSpec(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.value)

    def visit_ProcDecl(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.name)

        for fp in node.fpl:
            self.visit(fp)

        self.visit(node.block)

    def visit_Compound(self, node):
        self.log(f'{type(node).__name__}')

        for statement in node.statements:
            self.visit(statement)

    def visit_ProcCall(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.name)

        for ap in node.apl:
           self.visit(ap)

    def visit_Assign(self, node):
        self.log(f'{type(node).__name__}')

        self.visit(node.left)

        self.log(node.token.value)

        self.visit(node.right)

    def visit_BinOp(self, node):
        self.log(f'{type(node).__name__}')

        self.visit(node.left)

        self.log(node.token.value)

        self.visit(node.right)

    def visit_UnaryOp(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.factor)

        self.visit(node.left)

    def visit_Empty(self, node):
        self.log(f'{type(node).__name__}')
        pass

    def visit_Variable(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.value)

    def visit_Num(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.value)

    def build_ast(self):
        self.visit(self.root)







class Symbol(object):

    def __init__(self, name=None, type=None):
        self.name = name
        self.type = type
        self.scope_level = 0

class BuiltinTypeSymbol(Symbol):

    def __init__(self, name):
        super().__init__(name=name)

    def __str__(self):
        return f'{self.name}'

    def __repr__(self):
        return f'<BuiltinType:{self.name}>'

class ProgSymbol(Symbol):

    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return f'{self.name}'

    __repr__ = __str__

class ProcSymbol(Symbol):

    def __init__(self, name, fpl=None):
        super().__init__(name)
        self.fpl = [] if fpl==None else fpl
        self.ast_block = None #initalize value

    def __str__(self):
        fpl = [str(fp) for fp in self.fpl]
        return f'<{self.name}:{self.fpl}>'

    __repr__ = __str__

class VarSymbol(Symbol):

    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return f'<{self.name}:{self.type}>'

    __repr__ = __str__

 

class ScopeSymbolTable(object):

    def __init__(self, scope_name, scope_level, enclosing_scope):
        self._symbols = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope

    def log(self, msg):
        if _LOG_SST:
            print(msg)

    def _define_builtins(self):
        self.define(BuiltinTypeSymbol('INTEGER'))
        self.define(BuiltinTypeSymbol('REAL'))

    def define(self, symbol):
        self.log(f'Define: {symbol}')
        symbol.scope_level = self.scope_level
        self._symbols[symbol.name] = symbol

    def lookup(self, name, cur_scope_only=None):
        self.log(f'Lookup: {name}')
        symbol = self._symbols.get(name, None)

        if symbol != None:
            return symbol

        if cur_scope_only:
            return None

        return self.enclosing_scope.lookup(name)

    def __str__(self):
        lines = []

        line1 = '\nSCOPE SYMBOL TABLE:'
        line2 = len(line1) * '='

        lines.append(line1)
        lines.append(line2)

        if self.enclosing_scope == None:
            enclosing_scope_name = None
        else:
            enclosing_scope_name = self.enclosing_scope.scope_name    
        line3 = f'Scope name: {self.scope_name}'
        line4 = f'Scope level: {self.scope_level}'
        line5 = f'Enclosing scope: {enclosing_scope_name}'

        lines.append(line3)
        lines.append(line4)
        lines.append(line5)

        line6 = 'Scope Symbols'
        line7 = len(line6) * '-'

        lines.append(line6)
        lines.append(line7)

        for key, val in self._symbols.items():
            lines.append(f'{key} : {val}')

        return '\n'.join(lines) + '\n'


class SemanticAnalyzer(NodeVisitor):

    def __init__(self, root):
        self.root = root
        self.cur_scope = None

    def log(self, msg):
        if _LOG_SEMANTIC:
            print(f'{msg}')

    def visit_Program(self, node):
        prog_name = node.name
        prog_symbol = ProgSymbol(prog_name)
        
        prog_scope = ScopeSymbolTable(scope_name=prog_name,
                                      scope_level=self.cur_scope.scope_level + 1,
                                      enclosing_scope = self.cur_scope)

        self.log(f'ENTERING: {prog_name} scope')
        self.cur_scope = prog_scope

        self.visit(node.block)

        self.log(f'LEAVING: {prog_name} scope')
        self.log(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope

    def visit_Block(self, node):
        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_VarDecl(self, node):
        #check that the built-in type declared for the symbol
        #is a valid type in the language
        type_name = node.type_node.value
        type_symbol = self.cur_scope.lookup(type_name)

        var_name = node.var_node.value
        ###########
        #does this look back through multiple scopes?
        #should this be restricted to the current scope only?
        ###########
        #it should be restricted to the current scope; a variable cannot
        #be redeclared in the same scope, however it can be redeclared in
        #an inner scope effectively covering up a variable of same name
        #in an outer scope
        var_symbol = self.cur_scope.lookup(var_name, cur_scope_only=True)

        #check for duplicate symbols, i.e. a symbol with the same name
        #could be a VarSymbol or a ProcSymbol
        if var_symbol != None:
            raise SemanticError(error_code=ErrorCode.DUPLICATE_ID, 
                                msg=node.var_node.token)
      
        var_symbol = VarSymbol(var_name, type_symbol) 
        self.cur_scope.define(VarSymbol(var_name, type_symbol))  

        return var_symbol

    def visit_TypeSpec(self, node):
        pass

    def visit_ProcDecl(self, node):
        proc_name = node.name
        #########
        #shouldn't there be a lookup here to check for
        #duplicate symbol names in the current scope?
        ########
        #I think so, so I put one in.
        proc_symbol = self.cur_scope.lookup(proc_name, cur_scope_only=True)

        if proc_symbol != None:
            msg = f'The ID {proc_name} already exists in this scope.'
            raise SemanticError(error_code=ErrorCode.DUPLICATE_ID, 
                                msg=msg)

        proc_symbol = ProcSymbol(proc_name)
        self.cur_scope.define(proc_symbol)

        #add pointer from the ProcDecl Symbol to the Proc AST block
        #node for easy access by the ProcCall in the Interpreter
        proc_symbol.ast_block = node.block

        proc_scope = ScopeSymbolTable(scope_name=proc_name,
                                      scope_level=self.cur_scope.scope_level + 1,
                                      enclosing_scope=self.cur_scope)

        self.log(f'ENTERING: {proc_name} scope')
        self.cur_scope = proc_scope

        for fp in node.fpl:
            proc_symbol.fpl.append(self.visit(fp))

        self.visit(node.block)

        self.log(f'LEAVING: {proc_name} scope')
        self.log(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope

    def visit_Compound(self, node):
        for statement in node.statements:
            self.visit(statement)

    def visit_ProcCall(self, node):
        proc_name = node.name
     
        ######## 
        #should this be restricted to the current scope?
        ########
        #no, i see no reason why procedures in enclosing
        #scope can't be referenced in an inner scope
        #this also allows for the covering up of procedures
        #declared in outer scopes
        proc_symbol = self.cur_scope.lookup(proc_name)

        #pointer from ProcCall AST node to ProcDecl Symbol
        #to get access to the fpl and AST block node 
        #from the ProcCall in the Interpreter
        node.proc_symbol = proc_symbol
        
        num_fp = len(proc_symbol.fpl)
        num_ap = len(node.apl)

        for ap in node.apl:
            self.visit(ap)

    def visit_Assign(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_UnaryOp(self, node):
        self.visit(node.left)

    def visit_Empty(self, node):
        pass

    def visit_Variable(self, node):
        var_name = node.value
        var_symbol= self.cur_scope.lookup(var_name, cur_scope_only=True)

        #variable name resolution:
        #check if referened variable has been previously declared
        if var_symbol == None:
            raise SemanticError(error_codeErrorCode.ID_NOT_FOUND, 
                                msg=node.token)

        return var_symbol

    def visit_Num(self, node):
        pass

    def analyze(self):
        outer_scope = ScopeSymbolTable(
                        scope_name = 'outer_scope',
                        scope_level = 0,
                        enclosing_scope = self.cur_scope)

        outer_scope._define_builtins()

        self.log(f'ENTERING: outer_scope')
        self.cur_scope = outer_scope

        self.visit(self.root)

        self.log(f'LEAVING: outer_scope')
        self.log(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope
        




class ARType(Enum):
    PROGRAM = 'PROGRAM'
    PROCEDURE = 'PROCEDURE'


class CallStack(object):

    def __init__(self):
        self.stack = []

    def push(self, ar):
        self.stack.append(ar)

    def pop(self, ar):
        return self.stack.pop()

    def peek(self):
        return self.stack[-1]

    def __str__(self):
        lines = []

        line1 = 'CALL STACK'

        lines.append(line1)

        for ar in reversed(self.stack):
            lines.append(str(ar))

        return '\n'.join(lines) + '\n\n'

class ActivationRecord(object):

    def __init__(self, stack_name, stack_type, stack_level):
        self.members = {}
        self.stack_name = stack_name
        self.stack_type = stack_type
        self.stack_level = stack_level

    def __getitem__(self, key):
        return self.members[key]

    def __setitem__(self, key, val):
        self.members[key] = val

    def get(self, key):
        return self.members.get(key, None)

    def __str__(self):
        lines = []

        line1 = f'{self.stack_level}:{self.stack_type.value} {self.stack_name}'

        lines.append(line1)

        for key,val in self.members.items():
            lines.append(f'{key:15}={val}')

        return '\n'.join(lines) 


class Interpreter(NodeVisitor):

    def __init__(self, root):
        self.root = root
        self.call_stack = CallStack()

    def log(self, msg):
        if _LOG_INTERPRET:
            print(msg)

    def visit_Program(self, node):
        prog_name = node.name
        ar = ActivationRecord(stack_name=prog_name,
                              stack_type=ARType.PROGRAM,
                              stack_level=1)

        self.call_stack.push(ar)
        self.log(f'ENTER: PROGRAM {prog_name}')
        self.log(self.call_stack)

        self.visit(node.block)

        self.log(f'LEAVE: PROGRAM {prog_name}')
        self.log(self.call_stack)
        self.call_stack.pop(ar)

    def visit_Block(self, node):
        #I don't think we need to visit declarations
        #in the interpreter
        #for decl in node.declarations:
        #    self.visit(decl)

        self.visit(node.compound_statements)

    def visit_VarDecl(self, node):
        pass

    def visit_TypeSpec(self, node):
        pass

    def visit_ProcDecl(self, node):
        pass

    def visit_Compound(self, node):
        for statement in node.statements:
            self.visit(statement)

    def visit_ProcCall(self, node):
        proc_name = node.name

        proc_symbol = node.proc_symbol
        proc_block = proc_symbol.ast_block
        stack_level = proc_symbol.scope_level + 1

        ar = ActivationRecord(stack_name=proc_name,
                              stack_type=ARType.PROCEDURE,
                              stack_level=stack_level)

        fpl = proc_symbol.fpl
        apl = node.apl
        pdb.set_trace()
        for fp, ap in zip(fpl, apl):
            ar[fp.name] = self.visit(ap)

        self.call_stack.push(ar)
        self.log(f'ENTER: PROCEDURE {proc_name}')
        self.log(self.call_stack)

        self.visit(proc_block)

        self.log(f'LEAVE: PROCEDURE {proc_name}')
        self.log(self.call_stack)
        self.call_stack.pop(ar)

    def visit_Assign(self, node):
        left = node.left.value
        right = self.visit(node.right)

        ar = self.call_stack.peek()
        ar[left] = right

    def visit_BinOp(self, node):
        left = node.left
        right = node.right
        op = node.op.type

        if op == TokenType.ADD:
            return self.visit(left) + self.visit(right)
        if op == TokenType.SUB:
            return self.visit(left) - self.visit(right)
        if op == TokenType.MUL:
            return self.visit(left) * self.visit(right)
        if op == TokenType.INT_DIV:
            return self.visit(left) // self.visit(right)
        if op == TokenType.REAL_DIV:
            return self.visit(left) / self.visit(right)

    def visit_UnaryOp(self, node):
        factor = node.right
        op = node.left.type

        if op == TokenType.ADD:
            return +1*self.visit(factor)
        if op == TokenType.SUB:
            return -1*self.visit(factor)

    def visit_Empty(self, node):
        pass

    def visit_Variable(self, node):
        var_name = node.value
        ar = self.call_stack.peek()

        return ar[var_name]

    def visit_Num(self, node):
        num_type = node.token.type

        if num_type == TokenType.INT_CONST:
            return int(node.value)
        if num_type == TokenType.REAL_CONST:
            return float(node.value)

    def interpret(self):
        self.visit(self.root)




def main():

    text = open(r'19_a1.txt', 'r').read()

    global _LOG_PARSER, _LOG_VISUAL, _LOG_SST, _LOG_SEMANTIC, _LOG_INTERPRET

    _LOG_PARSER = _LOG_VISUAL = False
    _LOG_SST =  _LOG_SEMANTIC = _LOG_INTERPRET = True

    try:
        lexer = Lexer(text)
    except LexerError as e:
        print(e)
        sys.exit()

    #cur_token = lexer.get_next_token()

    #while cur_token.type != TokenType.EOF:
    #    print(cur_token)
    #    cur_token = lexer.get_next_token()

    #print(cur_token)

    try:
        parser = Parser(lexer)
        root = parser.parse()
    except ParserError as e:
        print(e)
        sys.exit()

    try:
        visual_ast = VisualAST(root)
        visual_ast.build_ast()
    except Exception as e:
        print(e)

    try:
        semantic_analyzer = SemanticAnalyzer(root)
        semantic_analyzer.analyze()
    except SemanticError as e:
        print(e)
        sys.exit()

    try:
        interpreter = Interpreter(root)
        interpreter.interpret()
    except Exception as e:
        raise(e)
        sys.exit()

if __name__ == '__main__':
    main()
