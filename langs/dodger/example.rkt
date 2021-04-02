#lang racket
(if (char? 42)
    (integer->char 955)
    (char->integer #\λ))
