#lang racket
;; TODO: replace with an expression that triggers the bug in `cons`
;; When interpreted, it should produce 1,
;; but when compiled it should produce 2.
(let ((x (cons 1 1))) (begin (box 2) (car x)))