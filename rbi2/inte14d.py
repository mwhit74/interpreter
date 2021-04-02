
"""
Lesson 14 - Part 4 
Source-to-source compiler

Translate a program in some source language to a program in the same
(or almost the same) source language. It seems like you can enhance or
tweak the source code for different applications. Possibly to increase
verbosity for human interpretation. 

I find this an interesting application but I don't really know what it 
would actually be used for. 

"""

from inte14_base import (
    Lexer,
    Parser,
    NodeVisitor,
    VisualAST,
    Symbol,
    BuiltinTypeSymbol,
    VarSymbol,
    ProcedureSymbol
    )

class ScopedSymbolTable(object):

    def __init__(self, scope_name, scope_level, enclosing_scope=None):
        self._symbols = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope

    def _init_builtins(self):
        self.insert(BuiltinTypeSymbol('INTEGER'))
        self.insert(BuiltinTypeSymbol('REAL'))

    def __str__(self):
        h1 = 'SCOPE (SCOPED SYMBOL TABLE)'
        lines = ['\n', h1, '='*len(h1)]
        for header_name, header_value in (
            ('Scope name', self.scope_name), 
            ('Scope level', self.scope_level),
            ('Enclosing Scope', self.enclosing_scope.scope_name if self.enclosing_scope else None)):

            lines.append(f'{header_name:15}: {header_value}')

        h2 = 'Scope (Scope symbol table) contents'
        lines.extend([h2, '-'*len(h2)])
        lines.extend(f'{key:7}: {value}' for key, value in self._symbols.items())
        lines.append('\n')
        s = '\n'.join(lines)
        return s

    __repr__ = __str__

    def insert(self, symbol):
        #print(f'Insert: {symbol.name}')
        self._symbols[symbol.name] = symbol

        """
        This was really confusing initially.

        I knew there had to be a class attribute somewhere to carry the scope
        but I couldn't find one in the class declaration. Then, I found a 
        reference further down var_symbol.scope and I just didn't understand.
        Finally, I ended up literally searching the source code and found this
        one line. 

        I didn't know this but apparently you can dynamically add attributes to
        classes after their declaration. That is what is happening below.

        We need to do this so the variables can be properly subscripted with 
        their scope level. 
        """
        symbol.scope = self

    def lookup(self, name, current_scope_only = False):
        print(f'Lookup: "{name}". (Scope name: {self.name})')

        #searches current scope level for variable
        symbol = self._symbols.get(name)
        #if the variable is found in the current scope level it is returned 
        if symbol != None:
            return symbol

        if current_scope_only:
            return None
        #if the variable is not found in the current scope level
        #the enclosing scope level, one level up, is searched until
        #either the variable is found or there are no more scopes to search
        if self.enclosing_scope != None:
            return self.enclosing_scope.lookup(name)


class ProgramSymbol(Symbol):

    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return f'<{self.__class__.__name__}(name={self.name})>'



class SrcToSrc(NodeVisitor):

    def __init__(self, root_node):
        self.root_node = root_node

        outer_scope = ScopedSymbolTable(
            scope_name = 'outer scope',
            scope_level = 0,
            enclosing_scope = None
        )

        outer_scope._init_builtins()
        self.cur_scope = outer_scope

        self.lines = []
        self.tl = 0

    def visit_Program(self, node):
        prog_symbol = ProgramSymbol(node.name)
        self.cur_scope.insert(prog_symbol)

        self.lines.append(f'program {node.name}{self.cur_scope.scope_level};')
        global_scope = ScopedSymbolTable(
            scope_name = 'global', 
            scope_level = 1, 
            enclosing_scope = self.cur_scope,
            )
        self.cur_scope = global_scope

        self.visit(node.block)

        self.lines.append(f'end. {{END OF {node.name}}}')

        #restores the previous scope level before leaving the current scope level
        self.cur_scope = self.cur_scope.enclosing_scope

    def visit_Block(self, node):
        self.tl += 1
        for decl in node.declarations:
            self.visit(decl)
        self.tl -= 1

        self.visit(node.compound_statements)

    def visit_Procedure(self, node):
        proc_str = self.tl*4*' '

        proc_name = node.name
        proc_symbol = ProcedureSymbol(proc_name)
        self.cur_scope.insert(proc_symbol)

        proc_str += f'procedure {proc_name}{self.cur_scope.scope_level}'


        proc_scope = ScopedSymbolTable(
            scope_name = proc_name,
            scope_level = self.cur_scope.scope_level + 1,
            enclosing_scope = self.cur_scope)
        self.cur_scope = proc_scope

        if len(node.params) > 0:
            c = 0
            proc_str += '('
            for param in node.params:
                param_type = self.cur_scope.lookup(param.type_node.value)
                param_name = param.var_node.value
                var_symbol = VarSymbol(param_name, param_type)
                self.cur_scope.insert(var_symbol)
                proc_symbol.params.append(var_symbol)

                proc_str += (f'{param_name}{var_symbol.scope.scope_level} : '
                             f'{param_type}{param_type.scope.scope_level}')

                if c != 0 and c < len(node.params) - 1:
                    proc_str += ','

                c += 1

            proc_str += ')'

        proc_str += f';'

        self.lines.append(proc_str)

        self.visit(node.block)
        
        self.lines.append(self.tl*4*' ' + f'end; {{END OF {proc_name}}}')

        self.cur_scope = self.cur_scope.enclosing_scope
        

    def visit_Param(self, node):
        #currently being handled by for-loop in visit_Procedure
        pass

    def visit_VarDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.cur_scope.lookup(type_name)

        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)

        #if there is already a symbol with var_name in the current scope
        #symbol table this error will be raised 
        if self.cur_scope.lookup(var_name, current_scope_only=True):
            raise Exception(f'Error: Duplicate identifier {var_name} found')

        self.cur_scope.insert(var_symbol)

        self.lines.append(self.tl*4*' ' + 
                            f'var {var_name}{var_symbol.scope.scope_level} :' +
                             f'{type_name}{type_symbol.scope.scope_level};')

    def visit_Variable(self, node):
        var_name = node.value
        var_symbol = self.cur_scope.lookup(var_name)

        type_symbol = var_symbol.type
        type_name = type_symbol.name

        if var_symbol is None:
            raise Exception(f'Error: Symbol(identifiier) not found {var_name}')

        if type_symbol is None:
            raise Exception(f'Error: Symbol(identifiier) not found {type_name}')

        return (f'<{var_name}{var_symbol.scope.scope_level}:' +
                f'{type_name}{type_symbol.scope.scope_level}>')

    def visit_TypeSpec(self, node):
        #currently being handled by visit_VarDecl
        pass

    def visit_Compound(self, node):
        self.lines.append(self.tl*4*' ' + 'begin')

        for statement in node.statements:
            self.visit(statement)

    def visit_Assign(self, node):
        self.tl += 1

        assign_str = self.tl*4*' '

        assign_str += self.visit(node.left) + ' := ' + self.visit(node.right) + ';'

        self.lines.append(assign_str)

        self.tl -= 1

    def visit_BinOp(self, node):
        return self.visit(node.left) + ' ' + str(node.op.value) + ' ' + self.visit(node.right)

    def visit_UnaryOp(self, node):
        return str(node.left) + self.visit(node.right)

    def visit_Num(self, node):
        return str(node.value)

    def visit_Empty(self, node):
        pass

    def analyze(self):
        self.visit(self.root_node)

        print('\n'.join(self.lines))


def main():
    text = open('a14_3.txt', 'r').read()

    lexer = Lexer(text)
    parser = Parser(lexer)
    tree = parser.parse()

    visual_ast = VisualAST(tree)
    visual_ast.build_ast()

    sa = SrcToSrc(tree)
    sa.analyze()


if __name__ == '__main__':
    main()
