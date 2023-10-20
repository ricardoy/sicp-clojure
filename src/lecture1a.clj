(ns lecture1a)

; approximation for square root of x
; make a guess g
; improve the guess by averaging g and x/g
; keep until the error is small
; use 1 as initial guess

(defn good-enough?
  [guess x]
  (<
    (abs (-
           (* guess guess)
           x))
    1e-10))

(defn average
  [x y]
  (/
    (+ x y)
    2))

(defn improve
  [guess x]
  (average guess (/ x guess)))

(defn try_
  [guess x]
  (if (good-enough? guess x)
    guess
    (try_ (improve guess x) x)))

(defn sqrt
  [x]
  (try_ 1.0 x))

(println (sqrt 0.1))

; exercise 1.6
(defn new-if
  [predicate then-clause else-clause]
  (cond
    predicate then-clause
    :else else-clause))

(defn sqrt-iter
  [guess x]
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;the new-if will evaluate the else clause even if the good-enough? is satisfied
;(println (sqrt-iter 1 4))