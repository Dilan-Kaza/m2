# CMSC 430 Midterm 2, Part 5


## Instructions

You've been provided a modified implementation of the Iniquity
language that was presented in class.

One observation about Iniquity that we made in class was that it is
possible to **syntactically** check if a program always calls
functions with the appropriate number of arguments.

For example, in the Iniquity program:
```
#lang racket
(define (f x y) x)
(f 100)
```
We can see that the application of `f` to `100` provides too few arguments.

The provided code updates the parser so that `parse` now calls
`parse-prog` to parse a sequence of s-expressions into a `Prog`
structure and then uses a helper predicate `correct-arity?` that is
given a `Prog` and return `#t` if that all applications in the given
program have the correct number of arguments and `#f` otherwise.  When
this predicate holds, the result of `parse` is just the same as
`parse-prog`, but when it doesn't hold it raises an error.  In other
words, we can make function applications with an incorrect number of
arguments into a parse error.

The provide code as a stub for `correct-arity?`, but you must complete
the implementation of it.



## Notes

  * We have not added any new tests.
  
  * For a program to pass `correct-arity?` **every** application must
    have the correct number of arguments, even if an application is
    unreachable.  For example, this program, when parsed and given to
    `correct-arity?` should produce `#f` even though there is no
    possibility of a run-time arity error.

```
#lang racket
(define (f x y) x)
(if #f (f 100) (f 1 2))
```

  * Any program that applies an undefined function has an arity error
    because you cannot determine what the appropriate number of
    arguments should be.  For example, this program should cause
    `correct-arity?` to produce `#f`:
	
```
#lang racket
(f 100)
```
