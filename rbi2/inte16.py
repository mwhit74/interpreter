"""
Lesson 16

"""

from collections import namedtuple
from enum import Enum
from string import whitespace
import pdb



class ErrorCode(Enum):

    UNEXPECTED_TOKEN    = 'Unexpected token'
    ID_NOT_FOUND        = 'Identifier not found'
    DUPLICATE_ID        = 'Duplicate id found'
    PROC_PARAMS         = 'Incorrect number of parameters passed'

class Error(Exception):

    def __init__(self, error_code=None, token=None, msg=None):
        self.error_code = error_code
        self.token = token
        self.msg = f'{self.__class__.__name__}: {msg}'

class LexerError(Error):
    pass

class ParserError(Error):
    pass

class SemanticError(Error):
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
    INT_TYPE    = 'INTEGER'
    REAL_TYPE   = 'REAL'
    PROCEDURE   = 'PROCEDURE'
    VAR         = 'VAR'
    BEGIN       = 'BEGIN'
    END         = 'END'
    INT_DIV     = 'DIV'

    ID          = 'ID'
    INT_CONST   = 'INT_CONST'
    REAL_CONST  = 'REAL_CONST'
    ASSIGN      = 'ASSIGN'
    EOF         = 'EOF'
 




Token = namedtuple('Token',['value', 'type', 'lineno', 'col'])




def _reserved_keywords():
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

RESERVED_KEYWORDS = _reserved_keywords()




class Lexer(object):

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]
        self.lineno = 1
        self.col = 1

    def error(self):
        msg = f'Lexer error on {self.cur_char}; lineno={self.lineno}, col={self.col}'

        raise LexerError(msg=msg)

    def get_next_char(self):
        if self.cur_char == '\n':
            self.lineno += 1
            self.col = 0

        self.pos += 1
        if self.pos < len(self.text) - 1:
            self.col += 1
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

        #get closing '}'
        value = value + self.cur_char
        self.get_next_char()

    def get_num(self):
        lineno = self.lineno
        col = self.col

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

            token_value = float(value)
            token_type =  TokenType.REAL_CONST

        else:
            token_value = int(value)
            token_type = TokenType.INT_CONST

        return Token(
                value=token_value,
                type=token_type,
                lineno=lineno,
                col=col)

    def get_id(self):
        lineno = self.lineno
        col = self.col

        value = ''
        while (self.cur_char != None and 
            (self.cur_char.isalnum() or self.cur_char == '_')):

            value = value + self.cur_char
            self.get_next_char()

        token_value = value.upper()

        token_type = RESERVED_KEYWORDS.get(token_value, TokenType.ID)

        return Token(
                value=token_value,
                type=token_type,
                lineno=lineno,
                col=col)

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
                token = Token(':=', TokenType.ASSIGN, self.lineno, self.col)
                self.get_next_char()
                self.get_next_char()
                return token
            try:
                token_type = TokenType(self.cur_char)
            except ValueError:
                self.error()
            else:
                token = Token(
                        value=token_type.value,
                        type=token_type,
                        lineno=self.lineno,
                        col=self.col)

                self.get_next_char()

                return token

        return Token('EOF',TokenType.EOF,self.lineno, self.col)








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

class ParamDecl(AST):

    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

class TypeSpec(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value

class Compound(AST):

    def __init__(self):
        self.statements = []

class Procedure(AST):

    def __init__(self, name, fpl, block):
        self.name = name
        self.fpl = fpl #formal parameter list
        self.block = block

class ProcedureCall(AST):

    def __init__(self, name, apl, token):
        self.name = name
        self.apl = apl #actual parameter list
        self.token = token

class BinOp(AST):

    def __init__(self, left, token, right):
        self.left = left
        self.token = self.op = token
        self.right = right

class UnaryOp(AST):

    def __init__(self, left, factor):
        self.left = self.op = left
        self.factor = factor

class Assign(AST):

    def __init__(self, left, token, right):
        self.left = left
        self.token = self.op = token
        self.right = right

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

    def error(self, error_code, token):
        raise ParserError(
                error_code=error_code,
                token=token,
                msg = f'{error_code.value} -> {token}')

    def check_token_type(self, token_type):
        print(self.cur_token)
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

        compound_statements = self.compound_statement()

        return Block(declarations, compound_statements)

    def declarations(self):
        """declarations: VAR (var_decl SEMI)+ |
                         (proc_decl)*
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
                

    def var_decl(self):
        """var_decl: variable (COMMA variable)* COLON type_spec"""

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
        """type_spec: REAL_TYPE | INT_TYPE"""

        token = self.cur_token

        if token.type == TokenType.REAL_TYPE:
            self.check_token_type(TokenType.REAL_TYPE)
        if token.type == TokenType.INT_TYPE:
            self.check_token_type(TokenType.INT_TYPE)

        return TypeSpec(token)

    def compound_statement(self):
        """compound_statement: BEGIN statement_list END"""

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

        if self.cur_token.type == TokenType.ID:
            self.error()

        return statements

    def statement(self):
        """statement: compound_statement |
                      assignment_statement |
                      empty_statment
        """

        token = self.cur_token

        if token.type == TokenType.BEGIN:
            return self.compound_statement()
        if token.type == TokenType.ID and self.lexer.cur_char == '(':
            return self.proccall_statement()
        if token.type == TokenType.ID:
            return self.assign_statement()
        else:
            return self.empty()

    def assign_statement(self):
        """assignment_statement: variable ASSIGN expr1"""
        left = self.variable()

        token = self.cur_token
        self.check_token_type(TokenType.ASSIGN)

        right = self.expr1()

        return Assign(left, token, right)

    def proc_decl(self):
        """procedure: PROCEDURE ID (OPAR formal_param_list CPAR)? SEMI block SEMI"""

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
        """formal_param_list: formal_param |
                              formal_param SEMI formal_param_list
        """

        fpl = self.formal_param()

        while self.cur_token.type == TokenType.SEMI:
            self.check_token_type(TokenType.SEMI)
            fpl.extend(self.formal_param())

        return fpl

    def formal_param(self):
        """formal_param: variable (variable COMMA)* COLON type_spec"""

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

    def proccall_statement(self):
        """proccall_statement: ID LPAREN (expr1 (COMMA expr1)*)? RPAREN"""
        token = self.cur_token
        name = self.cur_token.value
        self.check_token_type(TokenType.ID)

        self.check_token_type(TokenType.OPAR)

        if self.cur_token.type != TokenType.CPAR:
            apl = [self.expr1()]

            while self.cur_token.type == TokenType.COMMA:
                self.check_token_type(TokenType.COMMA)
                apl.append(self.expr1())

        self.check_token_type(TokenType.CPAR)

        return ProcedureCall(name, apl, token)
            

    def expr1(self):
        """expr1: expr2 ((ADD|SUB) expr2)*"""

        node = self.expr2()

        while self.cur_token.type in (TokenType.ADD,TokenType.SUB):
            token = self.cur_token
            if token.type == TokenType.ADD:
                self.check_token_type(TokenType.ADD)
            if token.type == TokenType.SUB:
                self.check_token_type(TokenType.SUB)

            node = BinOp(node, token, self.expr2())

        return node

    def expr2(self):
        """expr2: expr3 ((MUL|INT_DIV|REAL_DIV) expr3)*"""

        node = self.expr3()

        while self.cur_token.type in (TokenType.MUL,TokenType.INT_DIV,TokenType.REAL_DIV):
            token = self.cur_token
            if token.type == TokenType.MUL:
                self.check_token_type(TokenType.MUL)
            if token.type == TokenType.INT_DIV:
                self.check_token_type(TokenType.INT_DIV)
            if token.type == TokenType.REAL_DIV:
                self.check_token_type(TokenType.REAL_DIV)

            node = BinOp(node, token, self.expr3())

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
            node = self.expr1()
            self.check_token_type(TokenType.CPAR)
            return node
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

    def empty(self):
        """empty: """
        return Empty()

    def parse(self):
        root = self.program()
        if root == None:
            return None
        return root







class Symbol(object):

    def __init__(self, name, scope, type=None):
        self.name = name
        self.type = type
        self.scope = scope

class BuiltinTypeSymbol(Symbol):

    def __init__(self, name, scope):
        super().__init__(name, scope)

    def __str__(self):
        return f'{self.name}'

    def __repr__(self):
        return f'{self.__class__.__name__}<(name={self.name})>'

class VarSymbol(Symbol):

    def __init__(self, name, scope, type):
        super().__init__(name, scope, type=type)

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

    def __init__(self, scope_name, scope_level, enclosing_scope):
        self._symbols = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope

    def _init_builtins(self, scope):
        self.insert(BuiltinTypeSymbol('INTEGER', scope))
        self.insert(BuiltinTypeSymbol('REAL', scope))

    def insert(self, symbol):
        print(f'INSERT: {symbol.name}')
        self._symbols[symbol.name] = symbol

    def lookup(self, name, cur_scope_only = False):
        print(f'LOOKUP: {name} (Scope: {self.scope_name})')
        symbol = self._symbols.get(name, None)

        if symbol != None:
            return symbol

        if cur_scope_only: # and symbol == None
            return None

        return self.enclosing_scope.lookup(name)

    def __str__(self):
        lines = []

        line1 = '\nSCOPED SYMBOL TABLE'
        line2 = '='*len(line1)

        lines.append(line1)
        lines.append(line2)

        if self.enclosing_scope == None:
            enclosing_scope_name = None
        else:
            enclosing_scope_name = self.enclosing_scope.scope_name

        data = {'Scope Name':self.scope_name, 
                  'Scope Level':self.scope_level,
                  'Enclosing Scope':enclosing_scope_name}

        for key, val in data.items():
            lines.append(f'{key:15}: {val}')

        line3 = '\nScope contents'
        line4 = '-'*len(line3)

        lines.append(line3)
        lines.append(line4)

        for key,val in self._symbols.items():
            line = f'{key:7}: {val}'
            lines.append(line)

        out = '\n'.join(lines)

        return out
        

    __repr__ = __str__







class NodeVisitor(object):

    def visit(self, node):
        visitor_name = f'visit_{type(node).__name__}'
        visitor = getattr(self, visitor_name, self.visitor_default)
        return visitor(node)

    def visitor_default(self, node):
        raise Exception(f'Class has no method "visit_{type(node).__name__}".')







class SemanticAnalyzer(NodeVisitor):

    def __init__(self, root):
        self.root = root
        self.cur_scope = None

    def error(self, error_code, token):
        raise SemanticError(
                error_code=error_code, 
                token=token, 
                msg=f'{error_code.value} -> {token}')

    def visit_Program(self, node):
        name = node.name
        prog_symbol = ProgSymbol(name, self.cur_scope)
        self.cur_scope.insert(prog_symbol)

        prog_scope = ScopedSymbolTable(
                            scope_name = name,
                            scope_level = self.cur_scope.scope_level + 1,
                            enclosing_scope = self.cur_scope)

        print(f'ENTERING: {name} scope')
        self.cur_scope = prog_scope

        self.visit(node.block)

        print(f'LEAVING: {name} scope')
        print(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope

    def visit_Block(self, node):

        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_VarDecl(self, node):
        var_type = node.type_node.value
        type_symbol = self.cur_scope.lookup(var_type)

        var_name = node.var_node.value
        if self.cur_scope.lookup(var_name, cur_scope_only=True) != None:
            self.error(ErrorCode.DUPLICATE_ID, node.var_node.token)

        self.cur_scope.insert(VarSymbol(var_name, self.cur_scope, type_symbol))

    def visit_ParamDecl(self, node):
        var_type = node.type_node.value
        type_symbol = self.cur_scope.lookup(var_type)

        var_name = node.var_node.value

        if self.cur_scope.lookup(var_name, cur_scope_only=True) != None:
            self.error(ErrorCode.DUPLICATE_ID, node.var_node.token)

        var_symbol = VarSymbol(var_name, self.cur_scope, type_symbol)

        self.cur_scope.insert(var_symbol)

        return var_symbol

    def visit_TypeSpec(self, node):
        var_type = node.value
        return self.cur_scope.lookup(var_type)

    def visit_Compound(self, node):
        for statement in node.statements:
            self.visit(statement)

    def visit_Procedure(self, node):
        pdb.set_trace()
        name = node.name
        proc_symbol = ProcSymbol(name, self.cur_scope) 
        self.cur_scope.insert(proc_symbol)

        proc_scope = ScopedSymbolTable(
                        scope_name = name,
                        scope_level = self.cur_scope.scope_level + 1,
                        enclosing_scope = self.cur_scope)


        print(f'ENTERING: {name} scope')
        self.cur_scope = proc_scope

        for fp in node.fpl:
            proc_symbol.params.append(self.visit(fp))

        self.visit(node.block)

        print(f'LEAVING: {name} scope')
        print(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope

    def visit_ProcedureCall(self, node):
        proc_def = self.cur_scope.lookup(node.name)
        fpl_length = len(proc_def.params)

        apl_length = len(node.apl)

        if apl_length != fpl_length:
            raise self.error(
                    error_code=ErrorCode.PROC_PARAMS,
                    token = node.token)
                    
        for ap in node.apl:
            self.visit(ap)

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_UnaryOp(self, node):
        self.visit(node.right)

    def visit_Assign(self, node):
        self.visit(node.right)
        self.visit(node.left)

    def visit_Variable(self, node):
        var_node = self.cur_scope.lookup(node.value)
        if var_node == None:
            self.error(ErrorCode.ID_NOT_FOUND, node.token)

    def visit_Num(self, node):
        pass

    def visit_Empty(self, node):
        pass

    def analyze(self):

        outer_scope = ScopedSymbolTable(
                            scope_name='outer_scope',
                            scope_level = 0,
                            enclosing_scope = self.cur_scope)

        print('ENTERING: outer_scope')
        self.cur_scope = outer_scope

        outer_scope._init_builtins(outer_scope)

        self.visit(self.root)

        print('LEAVING: outer_scope')
        print(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope



class VisualAST(NodeVisitor):

    def __init__(self, root):
        self.root = root

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

        print(node.var_node.value) #could also call self.visit(node.var_node)
        print(node.type_node.value) #could also call self.visit(node.type_node)

    def visit_ParamDecl(self, node):
        print(type(node).__name__)

        print(node.var_node.value)
        print(node.type_node.value)

    def visit_TypeSpec(self, node):
        print(type(node).__name__)

        print(node.type_node.value)

    def visit_Compound(self, node):
        print(type(node).__name__)

        for statement in node.statements:
            self.visit(statement)

    def visit_Procedure(self, node):
        print(type(node).__name__)

        print(node.name)

        for fp in node.fpl:
            self.visit(fp)

        self.visit(node.block)

    def visit_ProcedureCall(self, node):
        print(type(node).__name__)

        print(node.name)

        for ap in node.apl:
            self.visit(ap)

    def visit_BinOp(self, node):
        print(type(node).__name__)

        self.visit(node.left)

        print(node.op)

        self.visit(node.right)

    def visit_UnaryOp(self, node):
        print(type(node).__name__)

        print(node.op)

        self.visit(node.right)

    def visit_Assign(self, node):
        print(type(node).__name__)

        self.visit(node.left)

        print(node.op)

        self.visit(node.right)

    def visit_Variable(self, node):
        print(type(node).__name__)

        print(node.value)

    def visit_Num(self, node):
        print(type(node).__name__)

        print(node.value)

    def visit_Empty(self, node):
        print(type(node).__name__)
        pass

    def buildast(self):
        self.visit(self.root)






"""
    def visit_Program(self, node):

    def visit_Block(self, node):

    def visit_VarDecl(self, node):

    def visit_ParamDecl(self, node):

    def visit_TypeSpec(self, node):

    def visit_Compound(self, node):

    def visit_Procedure(self, node):

    def visit_BinOp(self, node):

    def visit_UnaryOp(self, node):

    def visit_Assign(self, node):

    def visit_Variable(self, node):

    def visit_Num(self, node):

    def visit_Empty(self, node):

"""




def main():

    pdb.disable()

    text = open('a16_1.txt', 'r').read()

    lexer = Lexer(text)

    #try:
    #    cur_token = lexer.get_next_token()
    #    while cur_token.type != TokenType.EOF:
    #        print(cur_token)
    #        cur_token = lexer.get_next_token()

    #    print(cur_token) #prints EOF token
    #except LexerError as e:
    #    print(e.msg)

    try:
        parser = Parser(lexer)
        root = parser.parse()
    except (LexerError, ParserError) as e:
        print(e.msg)

    visualast = VisualAST(root)
    visualast.buildast()

    pdb.set_trace()
    try:
        analyzer = SemanticAnalyzer(root)
        analyzer.analyze()
    except SemanticError as e:
        print(e.msg)


if __name__ == '__main__':
    main()
