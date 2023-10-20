(ns lecture1b)

; define a function that evaluates the sum
; of squares of two numbers

(defn sq [x] (* x x))

(defn sos
  [x y]
  (+ (sq x) (sq y)))

(assert (= 2 (sos 1 1)))
(assert (= 13 (sos 2 3)))

; Peano arithmetic
; two ways to add whole numbers (natural)

(defn sum1
  [x y]
  (if (= x 0)
    y
    (recur (dec x) (inc y))))

(assert (= 4 (sum1 1 3)))
(assert (= 12 (sum1 4 8)))
(assert (= 56 (sum1 56 0)))

(defn sum2
  [x y]
  (if (= x 0)
    y
    (inc (sum2 (dec x) y))))

(assert (= 1 (sum2 1 0)))

; fibonacci
; 0 1 2 3 4 5 6 7  8  9  10 11
; 0 1 1 2 3 5 8 13 21 34 55 89

(defn fibo
  [n]
  (if (< n 2)
    n
    (+ (fibo (- n 1))
       (fibo (- n 2)))))

(assert (= 0 (fibo 0)))
(assert (= 1 (fibo 1)))
(assert (= 1 (fibo 2)))
(assert (= 21 (fibo 8)))

; hanoi tower
(defn move
  [n from to spare]
  (if (= n 0)
    (println "done")
    (
      (move (dec n) from spare to)
      (println "move" from to)
      (move (dec n) spare to from))))

(move 4 1 2 3)


; here starts the extra stuff, not in the class

; tail recursion version
(defn fibo-optimized
  [n]
  (if (< n 2)
    n
    (loop [a 0
           b 1
           k 2]
      (if (= k n)
        (+ a b)
        (recur b (+ a b) (inc k))))))

(assert (= 0 (fibo-optimized 0)))
(assert (= 1 (fibo-optimized 1)))
(assert (= 1 (fibo-optimized 2)))
(assert (= 21 (fibo-optimized 8)))

; tail recursion using clojure's trampoline

(defn fibo-trampoline
  [n]
  (letfn
     [(fibo-helper [a b n]
        (if (zero? n)
          (fn [] a)
          (fn [] (fibo-helper b (+ a b) (dec n)))))]
     (trampoline (fibo-helper 0N 1N n))))

;(println (fibo-trampoline 3))
(assert (= 0 (fibo-trampoline 0)))
(assert (= 1 (fibo-trampoline 1)))
(assert (= 1 (fibo-trampoline 2)))
(assert (= 21 (fibo-trampoline 8)))

(defn factorial-trampoline
  [n]
  (letfn
    [(factorial-helper [n acc]
       (if (zero? n)
         (fn [] acc)
         (fn [] (factorial-helper (dec n) (* acc n)))))]
    (trampoline (factorial-helper n 1))))

(assert (= 1 (factorial-trampoline 0)))
(assert (= 1 (factorial-trampoline 1)))
(assert (= 2 (factorial-trampoline 2)))
(assert (= 6 (factorial-trampoline 3)))
(assert (= 24 (factorial-trampoline 4)))

(defn factorial-trampoline2
  [n]
  (letfn
    [(factorial-helper [n acc]
       (if (zero? n)
         acc
         (fn [] (factorial-helper (dec n) (* acc n)))))]
    (trampoline #(factorial-helper n 1))))

(assert (= 1 (factorial-trampoline2 0)))
(assert (= 1 (factorial-trampoline2 1)))
(assert (= 2 (factorial-trampoline2 2)))
(assert (= 6 (factorial-trampoline2 3)))
(assert (= 24 (factorial-trampoline2 4)))

(println "end")