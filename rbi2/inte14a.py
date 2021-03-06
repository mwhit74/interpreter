


"""
Lesson 14 - Part 1
Nested Scopes 
(Also includes changes necessary to Lexer and Parser)

This iteration includes an update from a SymbolTable class to a
ScopedSymbolTable class which includes the scope name and a scope
level identifier. It also includes ability to parse Procedure 
Declarations including a list of formal parameters.

To provide more focus to the changes all the supporting classes
are in another file.

This is the first step to having a nested scope. However, the 
scope are not yet aware of each other and do not recognize
thier enclosing scopes. 

"""


from inte14_base import (
    Lexer,
    Parser,
    NodeVisitor,
    VisualAST,
    BuiltinTypeSymbol,
    VarSymbol,
    ProcedureSymbol
    )


class ScopedSymbolTable(object):

    def __init__(self, scope_name, scope_level):
        self._symbols = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self._init_builtins()

    def _init_builtins(self):
        self.insert(BuiltinTypeSymbol('INTEGER'))
        self.insert(BuiltinTypeSymbol('REAL'))

    def __str__(self):
        h1 = 'SCOPE (SCOPED SYMBOL TABLE'
        lines = ['\n', h1, '='*len(h1)]
        for header_name, header_value in (
            ('Scope name', self.scope_name), 
            ('Scope level', self.scope_level)):

            lines.append(f'{header_name:15}: {header_value}')

        h2 = 'Scope (Scope symbol table) contents'
        lines.extend([h2, '-'*len(h2)])
        lines.extend(f'{key:7}: {value}' for key, value in self._symbols.items())
        lines.append('\n')
        s = '\n'.join(lines)
        return s

    __repr__ = __str__

    def insert(self, symbol):
        print(f'Insert: {symbol.name}')
        self._symbols[symbol.name] = symbol

    def lookup(self, name):
        print(f'Lookup: {name}')
        symbol = self._symbols.get(name)
        return symbol





class SemanticAnalyzer(NodeVisitor):

    def __init__(self, root_node):
        self.cur_scope = None
        self.root_node = root_node

    def visit_Program(self, node):
        print(node.name)

        print('ENTER scope: global')
        global_scope = ScopedSymbolTable('global', 1)
        self.cur_scope = global_scope

        self.visit(node.block)

        print(global_scope)
        print('LEAVE scope: global')

    def visit_Block(self, node):
        for decl in node.declarations:
            self.visit(decl)

        self.visit(node.compound_statements)

    def visit_Procedure(self, node):
        proc_name = node.name
        proc_symbol = ProcedureSymbol(proc_name)
        self.cur_scope.insert(proc_symbol)

        print('ENTER scope: ' + proc_name)
        proc_scope = ScopedSymbolTable(proc_name, 2)
        self.cur_scope = proc_scope

        for param in node.params:
            param_type = self.cur_scope.lookup(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            self.cur_scope.insert(var_symbol)
            proc_symbol.params.append(var_symbol)

        self.visit(node.block)

        print(proc_scope)
        print('LEAVE scope: ' + proc_name)
        

    def visit_Param(self, node):
        #currently being handled by for-loop in visit_Procedure
        pass

    def visit_VarDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.cur_scope.lookup(type_name)

        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name,  type_symbol)
        self.cur_scope.insert(var_symbol)

    def visit_Variable(self, node):
        var_name = node.value
        var_symbol = self.cur_scope.lookup(var_name)

        if var_symbol is None:
            raise Exception(f'Error: Symbol(identifiier) not found {var_name}')

    def visit_TypeSpec(self, node):
        #currently being handled by visit_VarDecl
        pass

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

    def visit_Num(self, node):
        pass

    def visit_Empty(self, node):
        pass

    def analyze(self):
        self.visit(self.root_node)


def main():
    text = open('a14_1.txt', 'r').read()

    lexer = Lexer(text)
    parser = Parser(lexer)
    tree = parser.parse()

    visual_ast = VisualAST(tree)
    visual_ast.build_ast()

    sa = SemanticAnalyzer(tree)
    sa.analyze()


if __name__ == '__main__':
    main()
