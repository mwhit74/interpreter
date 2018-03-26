#Token types
#
#EOF (end-of-file) token is used to indicate that
#there is no more input left for lexical analysis
INTEGER, PLUS, MINUS, EOF = 'INTEGER', 'PLUS', 'MINUS', 'EOF'

class Token(object):
    def __init__(self, type, value):
        #token type: INTEGER, PLUS, EOF
        self. type = type
        #token value: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, '+', 'EOF'
        self.value = value

    def __str__(self):
        """String representation of class.
        Examples:
            Token(INTEGER, 3)
            Token(PLUS, '+')
        """
        return 'Token({type}, {value})'.format(type=self.type,
                                               value=repr(self.value))

    def __repr__(self):
        return self.__str__()


class Interpreter(object):
    def __init__(self, text):
        #client string input, e.g. 3+5
        self.text = text
        #self.pos is an index into self.text
        self.pos = 0
        self.current_token = None

    def error(self):
        raise Exception('Error parsing input')

    def advance(self):
        """Advance the 'pos' pointer and set the 'current_char'"""
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None #EOF
        else:
            self.current_char = self.text[self.pos]

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def integer(self):
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()
        return int(result)

    def get_next_token(self):
        """Lexical analyzer (also known as scanner or tokenizer)
        This method is reponsible for breaking a sentence apart into
        tokens. On token at a time.
        """

        while self.current_char is not None:
            if current_char.isspace():
                self.skip_whitespace()
                continue

            if current_char.isdigit():
                return Token(INTEGER, self.integer())
            
            if current_char == '+':
                self.advance()
                return Token(PLUS, '+')

            if curret_char == '-':
                self.advance()
                return Token(MINUS, '-')

            self.error()

        return Token(EOF,None)

    def check_type(self, token_type):
        #compare the current token type with token_type
        #and if they match get the next token

        #essentially type checking for the language
        if self.current_token.type == token_type:
            self.current_token = self.get_next_token()
        else:
            self.error()

    def expr(self):
        """expr -> INTEGER PLUS INTEGER
           expr -> INTEGER MINUS INTEGER"""
        #set current token to first token taken from the input
        self.current_token = self.get_next_token()

        #we expect the current token to be a single-digit integer
        left = self.current_token
        self.check_type(INTEGER)

        #we expect the current token to be a '+' token
        op = self.current_token
        if op.type == PLUS:
            self.check_type(PLUS)
        else:
            self.check_type(MINUS)

        #we expect the current token to be a single-digit
        right = self.current_token
        self.check_type(INTEGER)

        #after the above call the self.current_token is set to
        #EOF token

        #at this point INTEGER PLUS INTEGER sequence of tokens has
        #been successfully found and the method can just return
        #the result of adding two integers, thus effectively
        #interpreting client input
        if op.type == PLUS:
            result = left.value + right.value
        else:
            result = left.value - right.value
        return result

def main():
    while True:
        try:
            text = raw_input('calc> ')
        except EOFError:
            break
        if not text:
            continue
        interpreter = Interpreter(text)
        result = interpreter.expr()
        print(result)

if __name__ == "__main__":
main()
