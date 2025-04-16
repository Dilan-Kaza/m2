# CMSC 430 Midterm 2, Part 2


## Instructions

You've been provided a modified implementation of the Hustle language that was
presented in class.

In Hustle we implemented boxes, but they don't really _do_ anything. We've
decided it's time to let boxes shine, so we will be implementing a new
`set-box!` form that can modify the value stored inside a box.

Your `set-box!` should work similarly to Racket's:

  * The syntax is: `(set-box! e1 e2)`.
  * `e1` should evaluate to a box, _b_.
  * `e2` can evaluate to anything, _v_.
  * The contents of the box _b_ should be replaced with _v_.
  * The result of the expression itself is `#<void>`.

The AST types, parser, and interpreter have been updated to implement this new
form, and a stub has been added to the compiler. You must finish the compiler
implementation.

Implement the functionality in `compile-ops.rkt` so that `set-box!` works.


## Notes

  * A few very simple tests have been added to `test/test-runner.rkt`,
    which will all pass for the interpreter but fail for the compiler. These
    tests are **not** comprehensive. Write your own tests to check your work!

  * You do not need to write tests, but we highly encourage it!
