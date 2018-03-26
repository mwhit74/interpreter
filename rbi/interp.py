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

    def get_next_token(self):
        """Lexical analyzer (also known as scanner or tokenizer)
        This method is reponsible for breaking a sentence apart into
        tokens. On token at a time.
        """
        text = self.text

        #is self.pos index is past the end of the self.text ?
        #if so, then return EOF token because there is no more input
        #left to convert into tokens
        if self.pos > len(text) - 1:
            return Token(EOF, None)

        #get a character at the position self.pos and decide
        #what token to create based on the single char 
        current_char = text[self.pos]

        #if the char is a digit then convert it to integer,
        #create an INTEGER token, increment self.pos index to point
        #to the next char after the digit and return the INTEGER token
        if current_char.isdigit():
            token = Token(INTEGER, int(current_char))
            self.pos += 1
            return token
        
        if current_char == '+':
            token = Token(PLUS, current_char)
            self.pos += 1
            return token

        if curret_char == '-':
            token = Token(MINUS, current_char)
            self.pos += 1
            return token

        self.error()

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
        self.check_type(PLUS)

        #we expect the current token to be a single-digit
        right = self.current_token
        self.check_type(INTEGER)

        #after the above call the self.current_token is set to
        #EOF token

        #at this point INTEGER PLUS INTEGER sequence of tokens has
        #been successfully found and the method can just return
        #the result of adding two integers, thus effectively
        #interpreting client input
        result = left.value + right.value
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
