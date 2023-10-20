(ns chapter1-1)

;1.5

(defn p [] (p))

(defn test_ [x y]
  (if (= x 0)
    0
    y))

; (test_ 0 (p))
; stack overflow, therefore clojure uses an applicative-order evaluation

; 1.6

(defn new-if
  [predicate then-clause else-clause]
  (cond
    predicate then-clause
    :else else-clause))

(def then-procedure #(println "entered then"))
(def else-procedure #(println "entered else"))

;(let [x 0]
;  (if (= x 0)
;    (then-procedure)
;    (else-procedure)))

(let [x 0]
  (new-if (= x 0) then-procedure else-procedure))

; 1.7

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
    (recur (improve guess x) x)))

(defn sqrt
  [x]
  (try_ 1.0 x))

;(println
;  (sqrt 100000000000))

(defn proportion
  [guess new-guess]
  (/
     (abs (- guess new-guess))
     guess))

(defn optimized-good-enough?
  [guess x]
  (let [new-guess (improve guess x)]
    (<
      (proportion guess new-guess)
      1e-10)))

(defn try2
  [guess x]
  (if (optimized-good-enough? guess x)
    guess
    (recur (improve guess x) x)))

(defn sqrt2
  [x]
  (try2 1.0 x))

(println (sqrt2 4))

(println "end exercise 1-1")




