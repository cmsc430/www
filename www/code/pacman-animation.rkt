;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pacman-animation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; This one-off program makes a small PacMan animation of pacman eating pellets.

(require 2htdp/image)
(require 2htdp/universe)

;; Run to create GIF
(define (main _)
  (big-bang 0
    [on-tick add1]
    [to-draw show]
    [stop-when (λ (n) (= n (add1 (* 2 N))))]
    [record? #true]))

;; Number of steps for one half "chomp"
(define N 45)

;; show : Natural -> Scene
(define (show t)
  (overlay/align/offset "left" "middle"
                        (chomping-pacman t)
                        (/ SIZE 10)
                        0
                        (pellet-sequence t)))


(define SIZE 200)

(define HALF-CIRCLE
  (overlay/align "middle" "top"
                 (crop 0 0 SIZE (/ SIZE 2)
                       (circle (* 1/2 SIZE) "solid" "gold"))
                 (circle (* 1/2 SIZE) "solid" "transparent")))

(define PELLET
  (circle (/ SIZE 10) "solid" "salmon"))

(define PELLET-SPACE
  (rectangle (* 3 (/ SIZE 10)) 0 "solid" "red"))

(define PELLET-SEQUENCE
  (beside PELLET
          PELLET-SPACE
          PELLET
          PELLET-SPACE
          PELLET
          PELLET-SPACE
          PELLET
          PELLET-SPACE
          PELLET
          PELLET-SPACE
          PELLET))


;; make-pacman : Angle -> Image
;; Render pacman with mouth open at angle a (0 = closed).
(define (make-pacman a)
  (overlay (rotate a HALF-CIRCLE)
           (rotate (- a)
                   (flip-vertical HALF-CIRCLE))))

;; pellet-sequence : Natural -> Image
(define (pellet-sequence t)
  ;; 5*SIZE units per "loop"
  (crop (* (/ (modulo t N) N) (* 5 (/ SIZE 10)))
        0
        (+ (* (/ (modulo t N) N) (* 5 (/  SIZE 10)))
           (* 22 (/ SIZE 10)))
        (* 2 (/ SIZE 10))
        PELLET-SEQUENCE))



;0 1 2 3 4 5 6 7 8 9 10 11 ...
;0 1 2 3 4 5 4 3 2 1  0  1 ...

(define (count-up-and-down n)
  (+ n
     (* 2 (quotient n N)
        (- N n))))

(define (i n)
  (count-up-and-down (modulo n (* N 2))))


(define (chomping-pacman n)
  (make-pacman (i n)))

  

