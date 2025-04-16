# CMSC 430 Midterm 2, Part 3


## Instructions

You've been provided a modified implementation of the Hustle language that was
presented in class.

The only modification is that a bug has been introduced in the compilation
of the `cons` binary primitive.

A lazy Georgetown student copied and modified the code for `box` in
the implementation for `cons` and arrived at this code for compiling
`cons`:

```
;; Op2 -> Asm
(define (compile-op2 p)
  (match p
    ;...
    ['cons
     (seq (Mov (Offset rbx 0) rax)
          (Pop rax)
          (Mov (Offset rbx 8) rax)
          (Mov rax rbx)
          (Or rax type-cons)
          (Add rbx 8))])) ;; <--- BUG!  Should be 16
```

The problem is that this code only adjusts the `rbx` register by 8
instead of 16.

Your task is to write a program in `bug.rkt` that demonstrates this
bug in a specific way: when interpreted, the program should produce
the value `1`, but when compiled and executed, it should produce the
value `2`.

You do not need to modify the interpreter or compiler.  You only need
to write a Hustle program in `bug.rkt` that demonstrates the above
behavior.
