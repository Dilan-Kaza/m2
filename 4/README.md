# CMSC 430 Midterm 2, Part 4


## Instructions

You've been provided a modified implementation of the Hoax language
that was presented in class.

In Hoax we add strings to our language, but we didn't add very many
string operations.  Let's implement a very handy binary operations on
strings: `string=?`.  This operations takes two values which should be
strings (otherwise an error is signalled) and compares the strings for
structural equality, i.e. it returns `#t` if the strings consist of
exactly same sequence of characters and `#f` otherwise.

The AST types, parser, and interpreter have been updated to implement
this new primitive, and a stub has been added to the compiler.  You
must finish the compiler implementation.

Implement the functionality in `compile-ops.rkt` so that `string=?`
works.


## Notes

  * A few very simple tests have been added to `test/test-runner.rkt`,
    which will all pass for the interpreter but fail for the compiler. These
    tests are **not** comprehensive. Write your own tests to check your work!

  * You do not need to write tests, but we highly encourage it!


