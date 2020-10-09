#Token Types

INTEGER, PLUS, EOF = 'INTEGER', 'PLUS', 'EOF'

class Token(object):
    def __init__(self, type, value):
        #token type: INTEGER, PLUS, or EOF
        self.type = type
        #token value: 0-9, +, or None
        self.value = value

    def __str__(self):
        """String representation of class

        Examples:
            Token(INTEGER, 3)
            Token(PLUS, '+')
        """
        return f'Token({self.type}, {self.value}'

    def __repr__(self):
        return self.__str__()

class Interpreter(object):
    def __init__(self, text):
        #user string input
        self.text = text
        #self.pos is an index into self.text
        self.pos = 0
        #current token instance
        self.curent_token = None

    def error(self):
        raise Exception('Error parsing input')

    def get_next_token(self):
        """Lexical analyzer (also known as a scanner or tokenizer)

        This method is responsible for breaking a sentence apart
        into tokens. One token at a time.
        """
        text = self.text

        #is self.pos index past the end of the self.text?
        #if true, return EOF; no more input to tokenize
        if self.pos > len(self.text) - 1:
            return Token(EOF, None)

        #get a char at the position self.pos and decide what token
        #to create base on the single character
        cur_char = text[self.pos]

        #if the char is a digit then convert it to integer, create
        #an INTEGER token, increment self.pos index to point to 
        #next char after digit
        if cur_char.isdigit():
            token = Token(INTEGER, int(cur_char))
            self.pos += 1
            return token

        if cur_char == '+':
            token = Token(PLUS, cur_char)
            self.pos += 1
            return token

        self.error()

    def check_token_type(self, token_type):
        if self.cur_token.type == token_type:
            self.cur_token = self.get_next_token()
        else:
            self.error()

    def expr(self):
        """expr -> INTEGER PLUS INTEGER"""
        #set cur token to the first token taken from the input
        self.cur_token = self.get_next_token()

        #expect first token to be single digit int
        left = self.cur_token
        self.check_token_type(INTEGER)

        #expect second token to be '+' operator
        op = self.cur_token
        self.check_token_type(PLUS)

        #expect third token to be single digit int
        right = self.cur_token
        self.check_token_type(INTEGER)

        #at this point INTEGER PLUS INTEGER token sequence
        #has been successfully found and the method can 
        #return the result of adding two integer, thus
        #effectively interpreting client input
        result = left.value + right.value
        return result

def main():
    while True:
        try:
            text = input('calc>')
        except EOFError:
            break
        if not text:
            continue
        interpreter = Interpreter(text)
        result = interpreter.expr()
        print(result)

if __name__ == '__main__':
    main()

