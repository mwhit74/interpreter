#Token types
#
#EOF (end-of-file) token is used to indicate that there is no more input left
#lexial analysis

INTEGER, PLUS, EOF = 'INTEGER', 'PLUS', 'EOF'

class Token(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        """String representation of Token class"""
        return 'Token({type}, {value})'.format(type=self.type,
                                               value=repr(self.value))

    def __repr__(self):
        return self.__str__()


