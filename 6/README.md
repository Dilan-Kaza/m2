# CMSC 430 Midterm 2, Part 6


## Instructions

You've been provided a modified implementation of the Knock language that was
presented in class.  There is no compiler provided, since we did not cover
the Knock compiler in class.

We want to add a useful new pattern for our `match` expressions:
`list` patterns.

The `list` pattern takes any number of sub-patterns and matches values
which are lists of the appropriate length and where each element of the
list matches the appropriate sub-pattern.

For example, the pattern `(list _ _ _)` only matches lists of length 3.

The pattern `(list x y z)` only matches lists of length 3, and it binds
`x` to the first element, `y` to the second, and `z` to the third.

The pattern `(list 1 2 3)` only matches the value `(cons 1 (cons 2
(cons 3 '())))`.

The AST types and parser have been updated to recognize this new form
of patterns and a stub has been added to the interpreter, which you
must complete.


## Notes

  * We have added a few tests at the top of `test/test-runner.rkt`. We
    recommend writing tests of your own there.


