
from string import whitespace
from collections import namedtuple
import pdb

ADD, SUB, MUL, DIV = ('ADD', 'SUB', 'MUL', 'DIV')
NUM, OPAR, CPAR, EOF = ('NUM', 'OPAR', 'CPAR', 'EOF')
WS = whitespace

Token = namedtuple('Token', ['token_type', 'token_value'])

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

    def get_whitespace(self):
        value = ''
        while self.cur_char != None and self.cur_char in WS:
            value = value + self.cur_char
            self.get_next_char()

        return value

    def get_num(self):
        value = ''
        while self.cur_char != None and self.cur_char.isdigit():
            value = value + self.cur_char
            self.get_next_char()

        return int(value)

    def get_next_token(self):

        while self.cur_char != None:
            if self.cur_char in WS:
                value = self.get_whitespace()
                token = Token(WS, value)
            if self.cur_char.isdigit():
                value = self.get_num()
                return Token(NUM, value)
            if self.cur_char == '+':
                token = Token(ADD, self.cur_char)
                self.get_next_char()
                return token
            if self.cur_char == '-':
                token = Token(SUB, self.cur_char)
                self.get_next_char()
                return token
            if self.cur_char == '*':
                token = Token(MUL, self.cur_char)
                self.get_next_char()
                return token
            if self.cur_char == '/':
                token = Token(DIV, self.cur_char)
                self.get_next_char()
                return token
            if self.cur_char == '(':
                token = Token(OPAR, self.cur_char)
                self.get_next_char()
                return token
            if self.cur_char == ')':
                token = Token(CPAR, self.cur_char)
                self.get_next_char()
                return token

            self.error()

        return Token(EOF, None)

class AST(object):
    pass

class BinOp(AST):

    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = self.value = op
        self.right = right

class Num(AST):

    def __init__(self, token):
        self.left = None
        self.token = token
        self.value = token.token_value
        self.right = None

class Parser(object):

    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise SyntaxError('Invalid syntax.')

    def check_token_type(self, token_type):
        #print(self.cur_token)
        #pdb.set_trace()
        if self.cur_token.token_type == token_type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error()

    def expr1(self):

        node = self.expr2()

        while self.cur_token.token_type in (ADD, SUB):
            token = self.cur_token
            if token.token_type == ADD:
                self.check_token_type(ADD)
            elif token.token_type == SUB:
                self.check_token_type(SUB)

            node = BinOp(left=node, op=token, right=self.expr2())

        return node

    def expr2(self):

        node = self.expr3()

        while self.cur_token.token_type in (MUL, DIV):
            token = self.cur_token
            if token.token_type == MUL:
                self.check_token_type(MUL)
            elif token.token_type == DIV:
                self.check_token_type(DIV)

            node = BinOp(left=node, op=token, right=self.expr3())

        return node

    def expr3(self):
        token = self.cur_token
        if token.token_type == NUM:
            self.check_token_type(NUM)
            return Num(token)
        elif token.token_type == OPAR:
            self.check_token_type(OPAR)
            node = self.expr1()
            self.check_token_type(CPAR)
            return node

    def parse(self):
        return self.expr1()


class NodeVisitor(object):

    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))

class Interpreter(NodeVisitor):

    def __init__(self, parser):
        self.parser = parser

    def visit_BinOp(self, node):
        if node.op.token_type == ADD:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.token_type == SUB:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.token_type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.token_type == DIV:
            return self.visit(node.left) / self.visit(node.right) 

    def visit_Num(self, node):
        return node.value

    def interpret(self):
        tree = self.parser.parse() #root node
        lines = _build_tree_string(tree,0)[0]
        print('\n' + '\n'.join((line.rstrip() for line in lines)))
        return self.visit(tree)

        


#Borrowed this function from joowani/binarytree on github
def _build_tree_string(root, curr_index, index=False, delimiter='-'):
    """Recursively walk down the binary tree and build a pretty-print string.
    In each recursive call, a "box" of characters visually representing the
    current (sub)tree is constructed line by line. Each line is padded with
    whitespaces to ensure all lines in the box have the same length. Then the
    box, its width, and start-end positions of its root node value repr string
    (required for drawing branches) are sent up to the parent call. The parent
    call then combines its left and right sub-boxes to build a larger box etc.
    :param root: Root node of the binary tree.
    :type root: binarytree.Node
    :param curr_index: Level-order_ index of the current node (root node is 0).
    :type curr_index: int
    :param index: If set to True, include the level-order_ node indexes using
        the following format: ``{index}{delimiter}{value}`` (default: False).
    :type index: bool
    :param delimiter: Delimiter character between the node index and the node
        value (default: '-').
    :type delimiter:
    :return: Box of characters visually representing the current subtree, width
        of the box, and start-end positions of the repr string of the new root
        node value.
    :rtype: ([str], int, int, int)
    .. _Level-order:
        https://en.wikipedia.org/wiki/Tree_traversal#Breadth-first_search
    """
    if root is None:
        return [], 0, 0, 0

    line1 = []
    line2 = []
    if index:
        node_repr = '{}{}{}'.format(curr_index, delimiter, root.val)
    else:
        node_repr = str(root.value)

    new_root_width = gap_size = len(node_repr)

    # Get the left and right sub-boxes, their widths, and root repr positions
    l_box, l_box_width, l_root_start, l_root_end = \
        _build_tree_string(root.left, 2 * curr_index + 1, index, delimiter)
    r_box, r_box_width, r_root_start, r_root_end = \
        _build_tree_string(root.right, 2 * curr_index + 2, index, delimiter)

    # Draw the branch connecting the current root node to the left sub-box
    # Pad the line with whitespaces where necessary
    if l_box_width > 0:
        l_root = (l_root_start + l_root_end) // 2 + 1
        line1.append(' ' * (l_root + 1))
        line1.append('_' * (l_box_width - l_root))
        line2.append(' ' * l_root + '/')
        line2.append(' ' * (l_box_width - l_root))
        new_root_start = l_box_width + 1
        gap_size += 1
    else:
        new_root_start = 0

    # Draw the branch connecting the current root node to the right sub-box
    # Pad the line with whitespaces where necessary
    if r_box_width > 0:
        r_root = (r_root_start + r_root_end) // 2
        line1.append('_' * r_root)
        line1.append(' ' * (r_box_width - r_root + 1))
        line2.append(' ' * r_root + '\\')
        line2.append(' ' * (r_box_width - r_root))
        gap_size += 1
    new_root_end = new_root_start + new_root_width - 1

    # Combine the left and right sub-boxes with the branches drawn above
    gap = ' ' * gap_size
    new_box = [''.join(line1), ''.join(line2)]
    for i in range(max(len(l_box), len(r_box))):
        l_line = l_box[i] if i < len(l_box) else ' ' * l_box_width
        r_line = r_box[i] if i < len(r_box) else ' ' * r_box_width
        new_box.append(l_line + gap + r_line)

    # Return the new box, its width and its root repr positions
    return new_box, len(new_box[0]), new_root_start, new_root_end


def main():
    while True:
        try:
            text = input('spi>')
        except EOFError:
            break
        if not text:
            continue
        #pdb.set_trace()
        lexer = Lexer(text)
        parser = Parser(lexer)
        interpreter = Interpreter(parser)
        result = interpreter.interpret()
        print(result)

if __name__ == "__main__":
    main()
