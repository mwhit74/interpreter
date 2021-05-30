"""
Lesson 18

Executing procedure calls
"""

from collections import namedtuple
from string import whitespace
from enum import Enum
import pdb

pdb.disable()


class ErrorType(Enum):

    UNEXPECTED_TOKEN    = 'Unexpected Token'
    ID_NOT_FOUND        = 'ID not found'
    DUPLICATE_ID        = 'Duplicate ID'
    PROC_PARAMS         = 'Number of parameters does not match function definition'

class Error(Exception):

    def __init__(self, error_code=None, token=None, msg=None):
        self.error_code = error_code
        self.token = token
        self.msg = f'{self.__class__.__name__} msg: {msg}'

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

    ASSIGN      = 'ASSIGN'
    ID          = 'ID'
    EOF         = 'EOF'



Token = namedtuple('Token', ['type', 'value', 'line', 'col'])



def _reserved_keywords():
    tt_list = list(TokenType)
    start_index = tt_list.index(TokenType.PROGRAM)
    end_index = tt_list.index(TokenType.END)
    keywords = {tokentype.value: tokentype
                    for tokentype in tt_list[start_index:end_index+1]}

    return keywords

RESERVED_KEYWORDS = _reserved_keywords()




class Lexer(object):

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]

        self.col = 1
        self.line = 1

    def error(self):
        msg = f'Lexer error on "{self.cur_char}" line: {self.line} col: {self.col}'
        raise LexerError(msg=msg)

    def get_next_char(self):
        if self.cur_char == '\n':
            self.line += 1
            self.col = 0

        self.pos += 1
        self.col += 1
        if self.pos < len(self.text) - 1:
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
            value += self.cur_char
            self.get_next_char()

        #get closing '}'
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

            return Token(TokenType.REAL_CONST, float(value), self.line, self.col)

        return Token(TokenType.INT_CONST, int(value), self.line, self.col)

    def get_id(self):
        value = ''
        while (self.cur_char != None and 
                (self.cur_char.isalnum() or self.cur_char == '_')):

            value += self.cur_char
            self.get_next_char()

        value = value.upper()

        token_type = RESERVED_KEYWORDS.get(value, TokenType.ID)

        return Token(token_type, value, self.line, self.col)

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
                token = Token(TokenType.ASSIGN, ':=', self.line, self.col)
                self.get_next_char()
                self.get_next_char()
                return token
            try:
                token_type = TokenType(self.cur_char)
            except ValueError as e:
                self.error()
            else:
                token = Token(token_type, self.cur_char, self.line, self.col)
                self.get_next_char()
                return token

        return Token(TokenType.EOF, 'EOF', self.line, self.col)









class AST(object):
    pass

class Program(AST):

    def __init__(self, token, block):
        self.token = token
        self.name = self.token.value
        self.block = block

class Block(AST):

    def __init__(self, declarations, compound_statements):
        self.declarations = declarations
        self.compound_statements = compound_statements

class Compound(AST):

    def __init__(self):
        self.statements = []

class VarDecl(AST):

    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

class TypeSpec(AST):

    def __init__(self, token):
        self.token = token
        self.value = self.token.value

class ProcDecl(AST):

    def __init__(self, token, block, fpl):
        self.token = token
        self.name = self.token.value
        self.block = block
        self.fpl = fpl

class ParamDecl(AST):

    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

class ProcCall(AST):

    def __init__(self, token, apl, proc_symbol=None):
        self.token = token
        self.name = self.token.value
        self.apl = apl
        self.proc_symbol = proc_symbol

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

    def __init__(self, token, right):
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

    def log(self, msg):
        if _PARSER_LOG:
            print(msg)

    def error(self, error_code, token):
        msg = f'{error_code.value} -> {token}'
        raise ParserError(error_code, token, msg)

    def check_token_type(self, token_type):
        self.log(self.cur_token)
        if self.cur_token.type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error(ErrorType.UNEXPECTED_TOKEN, self.cur_token)

    def program(self):
        """program: PROGRAM ID SEMI block DOT"""
        self.check_token_type(TokenType.PROGRAM)

        token = self.cur_token
        self.check_token_type(TokenType.ID)
        self.check_token_type(TokenType.SEMI)

        block = self.block()

        self.check_token_type(TokenType.DOT)

        return Program(token, block)

    def block(self):
        """block: declarations compound_statement"""
        declarations = self.declaration() 

        compound_statements = self.compound_statement()

        return Block(declarations, compound_statements)

    def declaration(self):
        """declaration: VAR (var_decl SEMI)* | proc_decl"""

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
        """var_decl: ID (COMMA ID)* COLON type_spec"""

        var_nodes = [self.variable()]
        
        while self.cur_token.type == TokenType.COMMA:
            self.check_token_type(TokenType.COMMA)
            var_node.append(self.variable())

        self.check_token_type(TokenType.COLON)

        type_node = self.type_spec()

        var_decls = []
        for var_node in var_nodes:
            var_decls.append(VarDecl(var_node, type_node))

        return var_decls

    def type_spec(self):
        """type_spec: INTEGER|REAL"""

        token = self.cur_token

        if token.type == TokenType.INT_TYPE:
            self.check_token_type(TokenType.INT_TYPE)
        if token.type == TokenType.REAL_TYPE:
            self.check_token_type(TokenType.REAL_TYPE)

        return TypeSpec(token)

    def proc_decl(self):
        """PROCEDURE: ID (OPAR fpl CPAR)? SEMI block SEMI"""
        self.check_token_type(TokenType.PROCEDURE)

        token = self.cur_token
        self.check_token_type(TokenType.ID)

        fpl = []
        if self.cur_token.type == TokenType.OPAR:
            self.check_token_type(TokenType.OPAR)
            fpl = self.formal_param_list()
            self.check_token_type(TokenType.CPAR)

        self.check_token_type(TokenType.SEMI)

        block = self.block()

        self.check_token_type(TokenType.SEMI)

        return ProcDecl(token, block, fpl)

    def formal_param_list(self):
        """formal_param_list: (param_decl SEMI)* """

        fpl = self.param_decl()

        while self.cur_token.type == TokenType.SEMI:
            self.check_token_type(TokenType.SEMI)
            fpl.extend(self.param_decl())

        return fpl

    def param_decl(self):
        """param_decl: ID (COMMA ID)* COLON type_spec"""
        var_nodes = [self.variable()]

        while self.cur_token.type == TokenType.COMMA:
            self.check_token_type(TokenType.COMMA)
            var_nodes.append(self.variable())

        self.check_token_type(TokenType.COLON)

        type_node = self.type_spec()

        param_decls = []
        for var_node in var_nodes:
            param_decls.append(ParamDecl(var_node, type_node))

        return param_decls

    def compound_statement(self):
        """compound_statement: BEGIN statement_list END"""

        self.check_token_type(TokenType.BEGIN)

        statement_list = self.statement_list()

        self.check_token_type(TokenType.END)

        compound = Compound()
        for statement in statement_list:
            compound.statements.append(statement)

        return compound

    def statement_list(self):
        """statement_list: statement | statement SEMI statement_list"""

        statement_list = [self.statement()]

        while self.cur_token.type == TokenType.SEMI:
            self.check_token_type(TokenType.SEMI)
            statement_list.append(self.statement())

        if self.cur_token.type == TokenType.ID:
            self.error(ErrorType.UNEXPECTED_TOKEN, self.cur_token)

        return statement_list

    def statement(self):
        """statement: compound_statement |
                      assignment_statement |
                      proc_call |
                      empty
        """

        token = self.cur_token

        if token.type == TokenType.BEGIN:
            return self.compound_statement()
        if token.type == TokenType.ID and self.lexer.cur_char == '(':
            return self.proc_call()
        if token.type == TokenType.ID:
            return self.assign_statement()
        else:
            return self.empty()

    def assign_statement(self):
        """assign_statement: variable ASSIGN expr1 """
        left = self.variable()

        token = self.cur_token
        self.check_token_type(TokenType.ASSIGN)

        right = self.expr1()

        return Assign(left, token, right)

    def proc_call(self):
        """proc_call: ID OPAR apl CPAR"""
        token = self.cur_token
        self.check_token_type(TokenType.ID)

        apl = []
        self.check_token_type(TokenType.OPAR)
        if (self.cur_token.type in 
                (TokenType.ID, TokenType.REAL_CONST, TokenType.INT_CONST)):
            apl = self.actual_param_list()
        self.check_token_type(TokenType.CPAR)

        return ProcCall(token, apl)

    def actual_param_list(self):
        """actual_param_list: (expr1 COMMA)*"""
        apl = [self.expr1()]

        while self.cur_token.type == TokenType.COMMA:
            self.check_token_type(TokenType.COMMA)
            apl.append(self.expr1())

        return apl

    def expr1(self):
        """expr1: expr2 ((ADD|SUB) expr2)* """
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

    def empty(self):
        """empty: """
        return Empty()

    def parse(self):
        root = self.program()
        if root == None:
            return None
        return root






class NodeVisitor(object):

    def visit(self, node):
        name = f'visit_{type(node).__name__}'
        visitor = getattr(self, name, self.default_visitor)
        return visitor(node)

    def default_visitor(self, node):
        raise Exception(f'No class method name visit_{type(node).__name__} found.')




class Symbol(object):
    
    def __init__(self, name=None, type=None, scope=None):
        self.name = name
        self.type = type
        self.scope = scope

class BuiltinTypeSymbol(Symbol):

    def __init__(self, name, scope):
        super().__init__(name=name, scope=scope)

    def __str__(self):
        return f'{self.name}'

    def __repr__(self):
        return f'{self.__class__.__name__}<(name={self.name})>'

class ProgramSymbol(Symbol):

    def __init__(self, name, scope):
        super().__init__(name=name, scope=scope)

    def __str__(self):
        return f'{self.__class__.__name__}: <name={self.name}>'

    __repr__ = __str__

class VarSymbol(Symbol):

    def __init__(self, name, type, scope):
        super().__init__(name=name, type=type, scope=scope)

    def __str__(self):
        return f'{self.__class__.__name__}: <name={self.name}, type={self.type}>'

    __repr__ = __str__

class ProcSymbol(Symbol):

    def __init__(self, name, scope, block_ast, fpl=None):
        super().__init__(name=name, scope=scope)
        self.fpl = fpl if fpl!=None else []
        self.block_ast = block_ast

    def __str__(self):
        fpl = [str(fp) for fp in self.fpl]
        return f'{self.__class__.__name__}: <name={self.name}, params={params}>'

    __repr__ = __str__



class ScopedSymbolTable(object):

    def __init__(self, scope_name, scope_level, enclosing_scope):
        self._symbols = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope

    def log(self, msg):
        if _SCOPE_LOG:
            print(msg)

    def _init_builtins(self, scope):
        self.insert(BuiltinTypeSymbol('INTEGER', scope))
        self.insert(BuiltinTypeSymbol('REAL', scope))

    def insert(self, symbol):
        self.log(f'INSERT: symbol {symbol.name}, scope {symbol.scope.scope_name}')
        self._symbols[symbol.name] = symbol

    def lookup(self, name, cur_scope_only=False):
        self.log(f'LOOKUP: symbol {name}, scope {self.scope_name}')
        symbol = self._symbols.get(name, None)

        if symbol != None:
            return symbol

        if cur_scope_only:
            return None

        return self.enclosing_scope.lookup(name)

    def __str__(self):
        lines = []

        line1 = '\n\nSCOPE SYMBOL TABLE'
        line2 = '=' * len(line1)

        lines.append(line1)
        lines.append(line2)

        if self.enclosing_scope == None:
            enclosing_scope_name = None
        else:
            enclosing_scope_name = self.enclosing_scope.scope_name

        data = {'Scope name: ':self.scope_name,
                'Scope level: ':self.scope_level,
                'Enclosing scope: ':enclosing_scope_name}

        for key,val in data.items():
            lines.append(f'{key:15}: {val}')

        line3 = '\nScope Table Symbols'
        line4 = '-' * len(line3)

        lines.append(line3)
        lines.append(line4)

        for key,val in self._symbols.items():
            lines.append(f'{key:7}: {val}')

        return '\n'.join(lines) + '\n\n'

    __repr__ = __str__



class SemanticAnalyzer(NodeVisitor):

    def __init__(self, root):
        self.root = root
        self.cur_scope = None

    def log(self, msg):
        if _ANALYZER_LOG:
            print(msg)

    def visit_Program(self, node):
        prog_name = node.name
        prog_symbol = ProgramSymbol(prog_name, self.cur_scope)
        self.cur_scope.insert(prog_symbol)

        prog_scope = ScopedSymbolTable(
                            scope_name=prog_name,
                            scope_level=self.cur_scope.scope_level + 1,
                            enclosing_scope=self.cur_scope)

        self.log(f'ENTERING SCOPE: {prog_scope.scope_name}')
        self.cur_scope = prog_scope

        self.visit(node.block)

        self.log(f'LEAVING SCOPE: {self.cur_scope.scope_name}')
        self.log(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope

    def visit_Block(self, node):
        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_Compound(self, node):
        for statement in node.statements:
            self.visit(statement)

    def visit_VarDecl(self, node):
        var_type = node.type_node.value
        type_symbol = self.cur_scope.lookup(var_type)

        var_name = node.var_node.value

        var_symbol = VarSymbol(var_name, type_symbol, self.cur_scope)

        #if the same variable name exists in the current scope
        #raise an error
        if self.cur_scope.lookup(var_name, True) != None:
            token = node.var_node.token
            msg = f'{error_code.value} -> {token}'
            raise SemanticError(ErrorType.DUPLICATE_ID, token, msg)

        self.cur_scope.insert(var_symbol)

    def visit_TypeSpec(self, node):
        pass

    def visit_ProcDecl(self, node):
        proc_name = node.name
        proc_block = node.block
        proc_symbol = ProcSymbol(name=proc_name, 
                                 scope=self.cur_scope,
                                 block_ast = proc_block,
                                 fpl=None)
        #shouldn't there be a check against a duplicate
        #procedure name? 
        self.cur_scope.insert(proc_symbol)

        proc_scope = ScopedSymbolTable(
                            scope_name=proc_name,
                            scope_level=self.cur_scope.scope_level + 1,
                            enclosing_scope=self.cur_scope)

        self.log(f'ENTERING SCOPE: {proc_scope.scope_name}')
        self.cur_scope = proc_scope

        for fp in node.fpl:
            proc_symbol.fpl.append(self.visit(fp))

        self.visit(node.block)

        self.log(f'LEAVING SCOPE: {self.cur_scope.scope_name}')
        self.log(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope

    def visit_ParamDecl(self, node):
        var_type = node.type_node.value
        type_symbol = self.cur_scope.lookup(var_type)

        var_name = node.var_node.value

        var_symbol = VarSymbol(var_name, type_symbol, self.cur_scope)

        if self.cur_scope.lookup(var_name, True) != None:
            token = node.var_node.token
            msg = f'{error_code.value} -> {token}'
            raise SemanticError(ErrorType.DUPLICATE_ID, token, msg)

        self.cur_scope.insert(var_symbol)

        return var_symbol

    def visit_ProcCall(self, node):
        proc_name = node.name
        #shouldn't there be a check against looking up the
        #correct procedure symbol
        #I guess if it isn't found it will return none...
        proc_symbol = self.cur_scope.lookup(proc_name, cur_scope_only=True)

        if proc_symbol == None:
            msg = f'{error_code.value} -> {token}'
            raise SemanticError(ErrorType.ID_NOT_FOUND, node.token, msg)

        num_ap = len(node.apl)
        num_fp = len(proc_symbol.fpl)

        if num_ap != num_fp:
            msg = f'{error_code.value} -> {token}'
            raise SemanticError(ErrorType.PROC_PARAMS, node.token, msg) 

        for ap in node.apl:
            self.visit(ap)

        node.proc_symbol = proc_symbol

    def visit_Assign(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_UnaryOp(self, node):
        self.visit(node.right)

    def visit_Variable(self, node):
        var_name = node.value
        var_symbol = self.cur_scope.lookup(var_name)

        if var_symbol == None:
            msg = f'{error_code.value} -> {token}'
            raise SemanticError(ErrorType.ID_NOT_FOUND, node.token, msg)

    def visit_Num(self, node):
        pass

    def visit_Empty(self, node):
        pass

    def analyze(self):
        outer_scope = ScopedSymbolTable(
                            scope_name='outer',
                            scope_level=0,
                            enclosing_scope=self.cur_scope)

        outer_scope._init_builtins(outer_scope)
    
        self.log(f'ENTERING SCOPE: {outer_scope.scope_name}')
        self.cur_scope = outer_scope

        self.visit(self.root)

        self.log(f'LEAVING SCOPE: {self.cur_scope.scope_name}')
        self.log(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope







class CallStack(object):

    def __init__(self):
        self._records = []

    def push(self, ar):
        self._records.append(ar)

    def pop(self):
        return self._records.pop()

    def peek(self):
        return self._records[-1]

    def __str__(self):
        s = '\n'.join(repr(ar) for ar in reversed(self._records))
        s = f'CALL STACK\n{s}\n'
        return s

    __repr__ = __str__


class ARType(Enum):

    PROGRAM     = 'PROGRAM'
    PROCEDURE   = 'PROCEDURE'

class ActivationRecord(object):

    def __init__(self, name, type, nesting_level):
        self.members = {}
        self.nesting_level = nesting_level
        self.name = name
        self.type = type

    def __setitem__(self, key, val):
        self.members[key] = val

    def __getitem__(self, key):
        return self.members[key]

    def get(self, key):
        return self.members.get(key, None)

    def __str__(self):
        lines = []

        lines.append(f'{self.nesting_level}: {self.type.value} {self.name}')

        for key,val in self.members.items():
            lines.append(f'    {key:<20}: {val}')

        s = '\n'.join(lines)

        return s

    __repr__ = __str__


class Interpreter(NodeVisitor):

    def __init__(self, root):
        self.root = root
        self.call_stack = CallStack()

    def log(self, msg):
        if _INTERPRETER_LOG:
            print(msg)

    def visit_Program(self, node):
        prog_name = node.name
        prog_ar = ActivationRecord(
                        name=prog_name,
                        type=ARType.PROGRAM,
                        nesting_level=1)

        self.call_stack.push(prog_ar)

        self.log(f'ENTER: PROGRAM {prog_name}')
        self.log(str(self.call_stack))

        self.visit(node.block)

        self.log(f'LEAVE: PROGRAM {prog_name}')
        self.log(str(self.call_stack))
        self.call_stack.pop()

    def visit_Block(self, node):
        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_Compound(self, node):
        for statement in node.statements:
            self.visit(statement)

    def visit_VarDecl(self, node):
        pass

    def visit_TypeSpec(self, node):
        pass

    def visit_ProcDecl(self, node):
        pass

    def visit_ParamDecl(self, node):
        pass

    def visit_ProcCall(self, node):
        proc_name = node.name
        cur_ar = self.call_stack.peek()
        proc_ar = ActivationRecord(
                    name=proc_name,
                    type=ARType.PROCEDURE,
                    nesting_level=cur_ar.nesting_level + 1)

        proc_symbol = node.proc_symbol

        fps = proc_symbol.fpl
        aps = node.apl

        for ap, fp in zip(aps, fps):
            proc_ar[fp.name] = self.visit(ap) 

        self.call_stack.push(proc_ar)

        self.log(f'ENTER: PROC {proc_name}')
        self.log(str(self.call_stack))

        self.visit(proc_symbol.block_ast)

        self.log(f'LEAVING: PROC {proc_name}')
        self.log(str(self.call_stack))

        self.call_stack.pop()

    def visit_Assign(self, node):
        var_name = node.left.value
        var_value = self.visit(node.right)
        
        ar = self.call_stack.peek()
        ar[var_name] = var_value

    def visit_BinOp(self, node):
        left = node.left
        right = node.right

        if node.op.type == TokenType.ADD:
            return self.visit(left) + self.visit(right)
        if node.op.type == TokenType.SUB:
            return self.visit(left) - self.visit(right)
        if node.op.type == TokenType.MUL:
            return self.visit(left) * self.visit(right)
        if node.op.type == TokenType.INT_DIV:
            return self.visit(left) // self.visit(right)
        if node.op.type == TokenType.REAL_DIV:
            return self.visit(left) / self.visit(right)

    def visit_UnaryOp(self, node):
        if node.op.type == TokenType.ADD:
            return self.visit(node.right)
        if node.op.type == TokenType.SUB:
            return -1*self.visit(node.right)

    def visit_Variable(self, node):
        var_name = node.value

        ar = self.call_stack.peek()
        var_value = ar.get(var_name)

        return var_value

    def visit_Num(self, node):
        return node.value

    def visit_Empty(self, node):
        pass

    def interpret(self):
        self.visit(self.root)









class VisualAST(NodeVisitor):

    def __init__(self, root):
        self.root = root

    def log(self, msg):
        if _VISUAL_AST:
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

    def visit_Compound(self, node):
        self.log(f'{type(node).__name__}')

        pdb.set_trace()

        for statement in node.statements:
            self.visit(statement)

    def visit_VarDecl(self, node):
        self.log(f'{type(node).__name__}')

        self.visit(node.var_node)

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

    def visit_ParamDecl(self, node):
        self.log(f'{type(node).__name__}')

        self.visit(node.var_node)

        self.visit(node.type_node)

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

        self.log(node.token.value)

        self.visit(node.right)

    def visit_Variable(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.value)

    def visit_Num(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.value)

    def visit_Empty(self, node):
        self.log(f'{type(node).__name__}')
        pass

    def build_ast(self):
        self.visit(self.root)






def main():

    import sys

    text = open('a18_1.txt', 'r').read()

    global _PARSER_LOG, _VISUAL_AST, _ANALYZER_LOG, _SCOPE_LOG
    global _INTERPRETER_LOG

    _INTERPRETER_LOG = True
    _ANALYZER_LOG = _SCOPE_LOG = _VISUAL_AST = _PARSER_LOG = False

    try:
        lexer = Lexer(text)

        parser = Parser(lexer)
        root = parser.parse()

        #cur_token = lexer.get_next_token()

        #while cur_token.value != 'EOF':
        #    print(cur_token)
        #    cur_token = lexer.get_next_token()

        #print(cur_token)

    except (LexerError, ParserError) as e:
        print(e.msg)
        sys.exit()

    try:
        visual_ast = VisualAST(root)
        visual_ast.build_ast()
    except Exception as e:
        raise e

    try:
        analyzer = SemanticAnalyzer(root)
        analyzer.analyze()
    except SemanticError as e:
        print(e.msg)
        sys.exit()

    try:
        interpreter = Interpreter(root)
        interpreter.interpret()
    except InterpreterError as e:
        print(e.msg)
        sys.exit()


if __name__ == "__main__":
    main()
