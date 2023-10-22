(ns chapter1_2)

; 1.10
;1 5 10 25 50

(defn counting-change
  [total amounts]
  (if (empty? amounts)
    (if (zero? total) 1 0)
    (let [current (first amounts)]
      (if (> current total)
        (counting-change total (rest amounts))
        (+
          (counting-change (- total current) amounts)
          (counting-change total (rest amounts)))))))

(assert (= 1 (counting-change 1 [1])))
(println "ok1")

(assert (= 1 (counting-change 1 [1 3])))
(println "ok2")

(assert (= 2 (counting-change 2 [1 2])))
(println "ok3")

(assert (= 292 (counting-change 100 [1 5 10 25 50])))
(println "ok4")

; 1.11

(defn f
  [n]
  (if (< n 3)
    n
    (+
      (f (- n 1))
      (* 2 (f (- n 2)))
      (* 3 (f (- n 3))))))

(assert (= -1 (f -1)))
(assert (= 2 (f 2)))
(assert (= 4 (f 3)))
(assert (= 11 (f 4)))

(defn f-iter
  [n]
  (if (< n 3)
    n
    (loop [
           a 2
           b 1
           c 0
           n (- n 3)]
      (let [r (+ a (* 2 b) (* 3 c))]
        (if (zero? n)
          r
          (recur r a b (dec n)))))))

(assert (= -1 (f-iter -1)))
(assert (= 2 (f-iter 2)))
(assert (= 4 (f-iter 3)))
(assert (= 11 (f-iter 4)))

; 1.15

(defn cube
  [x]
  (* x x x))

(defn p
  "docstring"
  [x]
  (println "***")
  (- (* 3 x) (* 4 (cube x))))

(defn sine
  [angle]
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)


; GCD of two integers a and b is defined to be
; the largest integer that divides both a and b with no remainder

(defn gcd
  [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))

(assert (= 1 (gcd 4 3)))
(assert (= 2 (gcd 4 2)))
(assert (= 1 (gcd 5 2)))
