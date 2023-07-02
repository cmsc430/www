#lang crook
{:= E0 E1 F H0 H1 I}
(provide interp/io)
(require "interp.rkt")

{:> E0 I} ;; String Expr -> (Cons Value String)
{:> I}    ;; String Prog -> (Cons Value String)
{:> E0 I} ;; Interpret e with given string as input,
{:> I}    ;; Interpret p with given string as input,
;; return value and collected output as string
(define (interp/io {:> E0 I} e {:> I} p input)
  (parameterize ((current-output-port (open-output-string))
                 (current-input-port  (open-input-string input)))
      (cons (interp {:> E0 I} e {:> I} p)
            (get-output-string (current-output-port)))))
