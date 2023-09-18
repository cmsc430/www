#lang racket
(provide pre-installer)
(require crook/pi)

(define (pre-installer cs own)

  (define lang-name
    (let-values ([(b f d?) (split-path own)])
      (path->string f)))
  
  (main (path->string (collection-file-path "src/" "ziggy")) own
        (cdr (assoc lang-name '(#;("abscond"   . "A")
                                ("blackmail" . "B")
                                ("con"       . "C")
                                ("dupe"      . "D0")
                                ("dodger"    . "D1")
                                ("evildoer"  . "E0")
                                ("extort"    . "E1")
                                ("fraud"     . "F")
                                ("hustle"    . "H0")
                                ("hoax"      . "H1")
                                ("iniquity"  . "I")
                                ("jig"       . "J")
                                ("knock"     . "K"))))))
