"""
Lesson 17 

Updating the memory system with a call stack and activation records.

"""

from collections import namedtuple
from enum import Enum
from string import whitespace
import pdb



class ErrorType(Enum):
    UNEXPECTED_TOKEN    = 'Unexpected token'
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
    REAL_DIV   = '/'
    OPAR        = '('
    CPAR        = ')'
    
    DOT         = '.'
    COMMA       = ','
    SEMI        = ';'
    COLON       = ':'
    
    PROGRAM     = 'PROGRAM'
    INT_TYPE    = 'INTEGER'
    REAL_TYPE   = 'REAL'
    VAR         = 'VAR'
    PROCEDURE   = 'PROCEDURE'
    INT_DIV     = 'DIV'
    BEGIN       = 'BEGIN'
    END         = 'END'

    ID          = 'ID'
    ASSIGN      = 'ASSIGN'
    INT_CONST   = 'INT_CONST'
    REAL_CONST  = 'REAL_CONST'
    EOF         = 'EOF'



Token = namedtuple('Token', ['value', 'type', 'lineno', 'col'])




def _reserved_keywords():

    tt_list = list(TokenType)
    start_index = tt_list.index(TokenType.PROGRAM)
    end_index = tt_list.index(TokenType.END)
    keywords = { tokentype.value: tokentype 
                for tokentype in tt_list[start_index:end_index+1]}

    return keywords

RESERVED_KEYWORDS = _reserved_keywords()






class Lexer(object):

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[self.pos]
        self.lineno = 1
        self.col = 1

    def error(self, error_code, token):
        msg = f'Lexer error on {self.cur_char}; lineno = {self.lineno}, col = {self.col}'
        raise LexerError(msg=msg)

    def get_next_char(self):
        if self.cur_char == '\n':
            self.lineno += 1
            self.col = 0

        self.pos += 1
        self.col += 1
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

            return Token(
                    float(value), 
                    TokenType.REAL_CONST, 
                    self.lineno, 
                    self.col)

        else:
            return Token(
                    int(value), 
                    TokenType.INT_CONST, 
                    self.lineno, 
                    self.col)

    def get_id(self):
        value = ''
        while (self.cur_char != None and 
                (self.cur_char.isalnum() or self.cur_char == '_')):
            value += self.cur_char
            self.get_next_char()

        value = value.upper()

        token_type = RESERVED_KEYWORDS.get(value, TokenType.ID)

        token = Token(
                    value,
                    token_type,
                    self.lineno,
                    self.col)

        return token

    def get_next_token(self):

        while self.cur_char != None:
            if self.cur_char in whitespace:
                self.get_whitespace()
                continue
            if self.cur_char == "{":
                self.get_comment()
                continue
            if self.cur_char.isdigit():
                token = self.get_num()
                return token
            if self.cur_char.isalpha() or self.cur_char == '_':
                token = self.get_id()
                return token
            if self.cur_char == ':' and self.peek_next_char() == '=':
                token = Token(':=', TokenType.ASSIGN, self.lineno, self.col)
                self.get_next_char()
                self.get_next_char()
                return token
            try:
                token_type = TokenType(self.cur_char)        
            except ValueError as e:
                raise self.error(ErrorType.UNEXPECTED_TOKEN, token)
            else:
                token = Token(
                            token_type.value,
                            token_type,
                            self.lineno,
                            self.col)

                self.get_next_char()

                return token

        return Token('EOF', TokenType.EOF, self.lineno, self.col)








class AST(object):
    pass

class Program(AST):

    def __init__(self, name, block):
        self.name = name
        self.block = block

class Procedure(AST):

    def __init__(self, name, fpl, block):
        self.name = name
        self.fpl = fpl #formal parameter list
        self.block = block

class Block(AST):

    def __init__(self, declarations, compound_statements):
        self.declarations = declarations
        self.compound_statements = compound_statements

class Compound(AST):

    def __init__(self):
        self.statements = []

class Assign(AST):

    def __init__(self, left, token, right):
        self.left = left
        self.token = self.op = token
        self.right = right

class ProcCall(AST):

    def __init__(self, name, apl, token):
        self.name = name
        self.apl = apl #actual parameter list

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

    def __init(self, left, factor):
        self.left = left
        self.token = factor

class Empty(AST):
    pass



class Parser(object):

    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self, error_code, token):
        msg = f'{error_code.value} -> {token}'
        raise ParserError(error_code, token, msg)

    def log(self, msg):
        if _LOG_PARSER:
            print(msg)

    def check_token_type(self, token_type):
        self.log(self.cur_token)
        if self.cur_token.type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error(ErrorType.UNEXPECTED_TOKEN, self.cur_token)

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
        """type_spec: INT_TYPE | REAL_TYPE"""

        token = self.cur_token
        if token.type == TokenType.INT_TYPE:
            self.check_token_type(TokenType.INT_TYPE)
        if token.type == TokenType.REAL_TYPE:
            self.check_token_type(TokenType.REAL_TYPE)

        return TypeSpec(token)

    def proc_decl(self):
        """proc_decl: PROCEDURE ID (OPAR fpl CPAR)? SEMI block SEMI"""

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
        """formal_param: variable (COMMA variable)* COLON type_spec"""

        var_nodes = [self.variable()]

        while self.cur_token.type == TokenType.COMMA:
            self.check_token_type(COMMA)
            var_nodes.append(self.variable())

        self.check_token_type(TokenType.COLON)

        type_node = self.type_spec()

        fps = []
        for var_node in var_nodes:
            fps.append(ParamDecl(var_node, type_node))

        return fps
        

    def compound_statement(self):
        """compound_statement: BEGIN statement_list END"""
        self.check_token_type(TokenType.BEGIN)

        sl = self.statement_list()

        self.check_token_type(TokenType.END)

        compound = Compound()
        for statement in sl:
            compound.statements.append(statement)

        return compound

    def statement_list(self):
        """statement_list: statement |
                           statement SEMI statement_list
        """
        sl = [self.statement()]

        while self.cur_token.type == TokenType.SEMI:
            self.check_token_type(TokenType.SEMI)
            sl.append(self.statement())

        if self.cur_token.type == TokenType.ID:
            self.error(ErrorType.UNEXPECTED_TOKEN, self.cur_token)

        return sl

    def statement(self):
        """statement: compound_statement |
                      assign_statement |
                      proc_statement |
                      empty
        """
        pdb.set_trace()
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
        """assign_statement: variable ASSIGN expr1"""


        left = self.variable()

        token = self.cur_token
        self.check_token_type(TokenType.ASSIGN)

        right = self.expr1()

        return Assign(left, token, right)

    def proccall_statement(self):
        """proccall_statement: ID OPAR (apl)? CPAR"""

        name = self.cur_token.value
        token = self.cur_token
        self.check_token_type(TokenType.ID)

        self.check_token_type(TokenType.OPAR)

        if self.cur_token.type in (TokenType.ID, TokenType.REAL_CONST, TokenType.INT_CONST):
            apl = self.actual_param_list()

        self.check_token_type(TokenType.CPAR)

        return ProcCall(name, apl, token)

    def empty(self):
        """empty: """
        return Empty()

    def actual_param_list(self):
        """actual_param_list: expr 1 (COMMA expr1)*"""
        apl = [self.expr1()]

        while self.cur_token.type == TokenType.COMMA:
            self.check_token_type(TokenType.COMMA)
            apl.append(self.expr1())

        return apl

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
        """expr2: expr3 ((MUL|INT_DIV|REAL_DIV) epxr3)*"""
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
            return UnarOp(token, self.expr3())
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

        self.check_token_type(TokenType.EOF)

        if root is None:
            return None
        return root








class Symbol(object):

    def __init__(self, name, scope, type=None):
        self.name = name
        self.type = type
        self.scope = scope

class ProgSymbol(Symbol):

    def __init__(self, name, scope):
        super().__init__(name, scope)

    def __str__(self):
        return f'{self.__class__.__name__} <(name={self.name})>'

    __repr__ = __str__

class BuiltinTypeSymbol(Symbol):

    def __init__(self, name, scope):
        super().__init__(name, scope)

    def __str__(self):
        return f'{self.name}'

    def __repr__(self):
        return f'{self.__class__.__name__}<(name={self.name})>'

class VariableSymbol(Symbol):

    def __init__(self, name, scope, type):
        super().__init__(name, scope, type=type)

    def __str__(self):
        return f'{self.__class__.__name__}<(name={self.name}, type={self.type})>'

    __repr__ = __str__

class ProcSymbol(Symbol):

    def __init__(self, name, scope, params=None):
        super().__init__(name, scope)
        self.params = params if params != None else []

    def __str__(self):
        return f'{self.__class__.__name__}<(name={self.name}, params={self.params})>'

    __repr__ = __str__    

class ScopedSymbolTable(object):

    def __init__(self, name, level, enclosing_scope):
        self._symbols = {}
        self.scope_name = name
        self.scope_level = level
        self.enclosing_scope = enclosing_scope

    def _init_builtins(self, scope):
        self.insert(BuiltinTypeSymbol('INTEGER', scope))
        self.insert(BuiltinTypeSymbol('REAL', scope))

    def log(self, msg):
        if _LOG_SCOPE:
            print(msg)

    def insert(self, symbol):
        self.log(f'INSERT: symbol {symbol.name}, scope {symbol.scope.scope_name}')
        self._symbols[symbol.name] = symbol

    def lookup(self, name, cur_scope_only=False):
        self.log(f'LOOKUP: symbol {name}, scope {self.scope_name}')
        symbol = self._symbols.get(name, None)

        if symbol != None:
            return symbol

        if cur_scope_only: # and symbol == None
            return None

        return self.enclosing_scope.lookup(name)

    def __str__(self):
        lines = []

        line1 = '\n\nSCOPED SYMBOL TABLE'
        line2 = '='*len(line1)

        lines.append(line1)
        lines.append(line2)

        if self.enclosing_scope == None:
            enclosing_scope_name = None
        else:
            enclosing_scope_name = self.enclosing_scope.scope_name

        attrs = {'Scope name':self.scope_name,
                 'Scope level':self.scope_level,
                 'Enclosing scope':enclosing_scope_name}

        for key, val in attrs.items():
            lines.append(f'{key:15} : {val}')

        line3 = 'Symbols'
        line4 = '-'*len(line3)

        lines.append(line3)
        lines.append(line4)

        [lines.append(f'{key:7} : {val}') for key,val in self._symbols.items()]

        lines = '\n'.join(lines) + '\n\n'

        return lines

    __repr__ = __str__







class NodeVisitor(object):

    def visit(self, node):
        name = f'visit_{type(node).__name__}'
        visitor = getattr(self, name, self.default_visitor)
        return visitor(node)

    def default_visitor(self, node):
        raise Exception(f'Class does not have method {type(node).__name__}')




class SemanticAnalyzer(NodeVisitor):

    def __init__(self, root):
        self.root = root
        self.cur_scope = None

    def log(self, msg):
        if _LOG_SCOPE:
            print(msg)

    def visit_Program(self, node):
        name = node.name
        prog_symbol = ProgSymbol(name, self.cur_scope)
        self.cur_scope.insert(prog_symbol)

        prog_scope = ScopedSymbolTable(
                        name=name,
                        level=self.cur_scope.scope_level + 1,
                        enclosing_scope=self.cur_scope)

        self.log(f'ENTERING: {name} program scope')
        self.cur_scope = prog_scope

        self.visit(node.block)

        self.log(f'LEAVING: {name} program scope')
        self.log(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope

    def visit_Procedure(self, node):
        name = node.name
        proc_symbol = ProcSymbol(name, self.cur_scope)
        self.cur_scope.insert(proc_symbol)

        proc_scope = ScopedSymbolTable(
                        name=name,
                        level=self.cur_scope.scope_level + 1,
                        enclosing_scope=self.cur_scope)

        self.log(f'ENTERING: {name} procedure scope')
        self.cur_scope = proc_scope

        for fp in node.fpl:
            proc_symbol.params.append(self.visit(fp))

        self.visit(node.block)

        self.log(f'LEAVING: {name} procedure scope')
        self.log(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope

    def visit_Block(self, node):
        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_VarDecl(self, node):
        var_type = node.type_node.value #replaces call visit_TypeSpec
        type_symbol = self.cur_scope.lookup(var_type) #replaces call visit_TypeSpec

        var_name = node.var_node.value
        var_symbol = self.cur_scope.lookup(var_name, cur_scope_only=True)

        if var_symbol != None:
            raise SemanticError(ErrorCode.DUPLICATE_ID, node.var_node.token)

        self.cur_scope.insert(VariableSymbol(var_name, self.cur_scope, type_symbol))

    def visit_TypeSpec(self, node):
        pass

    def visit_ProcCall(self, node):
        proc_name = node.name
        proc_symbol = self.cur_scope.lookup(proc_name, cur_scope_only=True)

        if proc_symbol == None:
            raise SemanticError(ErrorCode.ID_NOT_FOUND, node.token)

        num_ap = len(node.apl)
        num_fp = len(proc_symbol.params)

        if num_ap != num_fp:
            raise SemanticError(ErrorCode.PROC_PARAMS, node.token)

        for ap in node.apl:
            self.visit(ap)

    def visit_ParamDecl(self, node):
        var_type = node.type_node.value #replaces call visit_TypeSpec
        type_symbol = self.cur_scope.lookup(var_type) #replaces call visit_TypeSpec

        var_name = node.var_node.value
        var_symbol = self.cur_scope.lookup(var_name, cur_scope_only=True)

        if var_symbol != None:
            raise SemanticError(ErrorCode.DUPLICATE_ID, node.var_node.token)

        var_symbol = VariableSymbol(var_name, self.cur_scope, type_symbol)

        self.cur_scope.insert(var_symbol)

        return var_symbol

    def visit_Compound(self, node):
        for statement in node.statements:
            self.visit(statement)

    def visit_Assign(self, node):
        self.visit(node.right)
        self.visit(node.left)

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_UnaryOp(self, node):
        self.visit(node.right)

    def visit_Variable(self, node):
        var_name = node.value
        var_symbol = self.cur_scope.lookup(var_name)

        if var_symbol == None:
            raise SemanticError(ErrorCode.ID_NOT_FOUND, node.var_node.token)

    def visit_Num(self, node):
        pass

    def visit_Empty(self, node):
        pass

    def analyze(self):

        outer_scope = ScopedSymbolTable(
                        name='outer',
                        level=0,
                        enclosing_scope=self.cur_scope)

        outer_scope._init_builtins(outer_scope)
        
        self.log(f'ENTERING: scope outer')
        self.cur_scope = outer_scope

        self.visit(self.root)

        self.log(f'LEAVING: scope {self.cur_scope.scope_name}')
        self.log(self.cur_scope)
        self.cur_scope = self.cur_scope.enclosing_scope





class CallStack:

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

    def __repr__(self):
        return self.__str__()


class ARType(Enum):

    PROGRAM = 'PROGRAM'
    PROCEDURE = 'PROCEDURE'

class ActivationRecord:

    def __init__(self, name, type, nesting_level):
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.members = {}

    def __setitem__(self, key, value):
        self.members[key] = value

    def __getitem__(self, key):
        return self.members[key]

    def get(self, key):
        return self.members.get(key)

    def __str__(self):
        lines = [f'{self.nesting_level}: {self.type.value} {self.name}']

        for name, val in self.members.items():
            lines.append(f'    {name:<20}: {val}')

        s = '\n'.join(lines)

        return s

    def __repr__(self):
        return self.__str__()






class Interpreter(NodeVisitor):


    def __init__(self, root):
        self.root = root
        self.call_stack = CallStack()

    def log(self, msg):
        if _LOG_STACK:
            print(msg)

    def visit_Program(self, node):
        prog_name = node.name

        self.log(f'ENTER: PROGRAM {prog_name}')        

        prog_ar = ActivationRecord(
                        name=prog_name,
                        type=ARType.PROGRAM,
                        nesting_level=1)

        self.call_stack.push(prog_ar)
        self.log(str(self.call_stack))

        self.visit(node.block)

        self.log(f'LEAVE: PROGRAM {prog_name}')
        self.log(str(self.call_stack))

        self.call_stack.pop()

    def visit_Procedure(self, node):
        pass

    def visit_Block(self, node):
        self.visit(node.compound_statements)

    def visit_VarDecl(self, node):
        pass

    def visit_TypeSpec(self, node):
        pass

    def visit_ProcCall(self, node):
        pass

    def visit_ParamDecl(self, node):
        pass

    def visit_Compound(self, node):
        for statement in node.statements:
            self.visit(statement)

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
            return +1*self.visit(node.right)
        if node.op.type == TokenType.SUB:
            return -1*self.visit(node.right)

    def visit_Variable(self, node):
        var_name = node.value
        ar = self.call_stack.peek()
        return ar.get(var_name)

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
        if _LOG_AST:
            print(msg)

    def visit_Program(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.name)

        self.visit(node.block)

    def visit_Procedure(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.name)

        for fp in node.fpl:
            self.visit(fp)

        self.visit(node.block)

    def visit_Block(self, node):
        pdb.set_trace()
        self.log(f'{type(node).__name__}')

        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_VarDecl(self, node):
        self.log(f'{type(node).__name__}')

        self.visit(node.var_node)
        self.visit(node.type_node)

    def visit_TypeSpec(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.value)

    def visit_ProcCall(self, node):
        self.log(f'{type(node).__name__}')

        self.log(node.name)

        for ap in node.apl:
            self.visit(ap)

    def visit_ParamDecl(self, node):
        self.log(f'{type(node).__name__}')

        self.visit(node.var_node)
        self.visit(node.type_node)

    def visit_Compound(self, node):
        pdb.set_trace()
        self.log(f'{type(node).__name__}')

        for statement in node.statements:
            self.visit(statement)

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

        self.log(token.value)

        self.visit(node.right)

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

"""
    def visit_Program(self, node):

    def visit_Procedure(self, node):

    def visit_Block(self, node):

    def visit_VarDecl(self, node):

    def visit_TypeSpec(self, node):

    def visit_ProcCall(self, node):

    def visit_ParamDecl(self, node):

    def visit_Compound(self, node):

    def visit_Assign(self, node):

    def visit_BinOp(self, node):

    def visit_UnaryOp(self, node):

    def visit_Variable(self, node):

    def visit_Num(self, node):

    def visit_Empty(self, node):
"""



def main():
    import sys
    import argparse

    parser = argparse.ArgumentParser(
        description='SPI - Simple Pascal Interpreter'
        )

    #parser.add_argument('inputfile', help='Pascal souce file')
    parser.add_argument(
        '--parser',
        help='Print parser tokens',
        action='store_true',
        )
    parser.add_argument(
        '--scope',
        help='Print scope information',
        action='store_true',
        )
    parser.add_argument(
        '--stack',
        help='Print call stack',
        action='store_true',
        )
    parser.add_argument(
        '--ast',
        help='Print AST',
        action='store_true',
        )
    parser.add_argument(
        '--all',
        help='Print all logs',
        action='store_true'
        )

    args = parser.parse_args()

    global _LOG_PARSER, _LOG_SCOPE, _LOG_STACK, _LOG_AST, _LOG_ALL

    _LOG_PARSER, _LOG_SCOPE, _LOG_STACK, _LOG_AST, _LOG_ALL = (args.parser, args.scope, 
                                                                args.stack, args.ast, args.all)

    if _LOG_ALL:
        _LOG_PARSER= _LOG_SCOPE= _LOG_STACK= _LOG_AST = True



    pdb.disable()
    text = open('a17_1.txt', 'r').read()

    try:
        lexer = Lexer(text)
    except LexerError as e:
        print(e.msg)
        sys.exit()

    #cur_token = lexer.get_next_token()
    #print(cur_token)

    #while cur_token.type != TokenType.EOF:
    #    cur_token = lexer.get_next_token()
    #    print(cur_token)

    pdb.set_trace()
    try:
        parser = Parser(lexer)
        root = parser.parse()
    except ParserError as e:
        print(e.msg)
        sys.exit()

    pdb.set_trace()
    visualast = VisualAST(root)
    visualast.build_ast()

    try:
        semantic_analyzer = SemanticAnalyzer(root)
        semantic_analyzer.analyze()
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
