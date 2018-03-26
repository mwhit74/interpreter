---disclaimer---

I am not sure if this is so much a readme as it is notes, thoughts, and a place
to store research. I may in the future start a project wiki to house all this
stuff but for now this is my quick and dirty solution.

---disclaimer---

This is an ongoing project to help me learn how to write an interperter. This is
a daunting task for someone with no real CS background. I have a tenuious grasp
of grammers and how that helps to define a language.

I don't really want to read a whole book on language theory and Turing machines.
I want a working knowledge and maybe someday a theoretical knowledge.

The end goal of this project is a very flexible way to accept text file input
from users. I think it is possible to write a "language" for an analysis program
that the user can learn. I think this can be very powerful, flexible, scalable,
and efficient way to accept user input via a text file. It's not a glamorous and
shining as a GUI but it does the job much better in my opinion. 

GUIs are great for some programs but in my experience with engineering analysis 
and design, program users want very good control over what the program is doing.
How do you offer that control? Basically provide them an API of commands to run
the program as they see fit. You can build in functions and tools that offer an 
expedited way to set up the model/geometry/analysis but it is up to the user to
use that function or tool. You also have to provide them a guide about what has
to be defined and when for the program to run properly, e.g. you can call
function bar without first defining parameter foo. 

I have considered writing a text parser with regex but I think that there will
be some limitations. I may come back to this idea because I think there has to
be some kind of "lexer" and I may have to write something like this. I think the
limitations I am considering focus mainly around the flexibility. If I want to
implement a new function or parameter I have to re-write or add to the already
complex regex. The regex will have to include all the keywords to be able to
break the input file apart which is inherently not a flexible solution. You are
hard-coding keywords into the program, generally not a good solution. 

Links:

http://compilers.iecc.com/crenshaw/tutor2.txt

http://forums.devshed.com/programming-languages-139/interpreter-compiler-312483.html#post1342279

http://www.i-programmer.info/bookreviews/20-theory/35-a-concise-introduction-to-languages-and-machines.html

https://learnpythonthehardway.org/book/ex48.html

http://parsingintro.sourceforge.net/

https://ruslanspivak.com/lsbasi-part1/
