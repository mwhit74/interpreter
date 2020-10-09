from inte1b import Interpreter

ex = Interpreter('12+3')
print(ex.get_next_token())
print(ex.get_next_token())
print(ex.get_next_token())

ui = input()

ex.expr()
