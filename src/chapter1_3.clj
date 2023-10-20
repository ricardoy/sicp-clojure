(ns chapter1-3)

; procedures as arguments

(defn sum-integers
  "compute the sum of the integers from a through b"
  [a b]
  (if (> a b)
    0
    (+
      a
      (sum-integers (+ a 1) b))))

(defn cube
  [x]
  (* x x x))

(defn sum-cubes
  [a b]
  (if (> a b)
    0
    (+
      (cube a)
      (sum-cubes (+ a 1) b))))

(defn pi-sum
  [a b]
  (if (> a b)
    0
    (+
      (/ 1.0 (* a (+ a 2)))
      (pi-sum (+ a 4) b))))

; the examples above follow the sigma notation for sum

(defn sum
  [term a next b]
  (if (> a b)
    0
    (+
      (term a)
      (sum term (next a) next b))))

(defn new-sum-cubes
  [a b]
  (sum cube a inc b))

(assert (= 3025 (new-sum-cubes 1 10)))

(defn integral
  [f a b dx]
  (letfn [(add-dx [x] (+ x dx))]
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx)))

(println (integral cube 0 1 0.01))
(println (integral cube 0 1 0.001))

; exercise 1.29



(defn simpson-rule
  [f a b n]
  (let [h (/ (- b a) n)]
    (if (zero? n)
      (* (/ h 3) (f a))
      (*
        (/ h 3)
        (letfn [(term [k]
                  (*
                    (if (odd? k) 4 2)
                    (f (+ a (* k h)))))]
          (sum term 0 inc n))))))

(println (simpson-rule cube 0. 1. 100))
(println (simpson-rule cube 0. 1. 1000))

; exercise 1.30

;(defn sum
;  [term a next b]
;  (if (> a b)
;    0
;    (+
;      (term a)
;      (sum term (next a) next b))))

;this raises a StackOverflowError
;(sum (fn [x] x) 1 inc 10000)

(defn sum_
  [term a next b]
  (letfn [(iter
            [a result]
            (if (> a b)
              result
              (recur (next a) (+ result (term a)))))]
    (iter a 0)))

(println (sum_ (fn [x] x) 1 inc 1000))

; exercise 1.32

(defn accumulate-recursive
  [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner
      (term a)
      (accumulate-recursive combiner null-value term (next a) next b))))

(defn sum-accumulate-recursive
  [a b]
  (accumulate-recursive + 0 (fn [x] x) a inc b))

(println (sum-accumulate-recursive 1 1000))


(defn accumulate-iter
  [combiner null-value term a next b]
  (loop [a a
         result null-value]
    (if (> a b)
      result
      (recur (next a) (combiner result (term a))))))

(defn sum-accumulate-iter
  [a b]
  (accumulate-iter + 0 (fn [x] x) a inc b))

(println (sum-accumulate-iter 1 10000))


; exercise 1.33)

(defn filtered-accumulate
  [combiner null-value term a next b predicate]
  (loop [a a
         result null-value]
    (if (> a b)
      result
      (if (predicate (term a))
        (recur (next a) (combiner result (term a)))
        (recur (next a) result)))))

(defn sum-filtered-accumulate-odd
  [a b]
  (filtered-accumulate + 0 (fn [x] x) a inc b even?))

(assert (= 6 (sum-filtered-accumulate-odd 0 4)))

; half-interval method

(defn average
  [a b]
  (/ (+ a b) 2))

(defn close-enough?
  [x y]
  (< (abs (- x y)) 0.00001))

(defn positive?
  [x]
  (> x 0))

(defn negative?
  [x]
  (< x 0))

(defn search
  [f neg-point pos-point]
  (let [midpoint (average neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
        midpoint
        (let [test-value (f midpoint)]
          (cond
            (positive? test-value) (search f neg-point midpoint)
            (negative? test-value) (search f midpoint pos-point)
            :else midpoint)))))

(defn half-interval-method
  [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond
      (and (negative? a-value) (positive? b-value)) (search f a b)
      (and (negative? b-value) (positive? a-value)) (search f b a)
      :else (throw (AssertionError. "Values are not of opposite sign")))))

(println (half-interval-method (fn [x] (Math/sin x)) 2.0 4.0))

; fixed point

(defn fixed-point
  [f first-guess]
  (let [tolerance 0.00001]
    (letfn [(close-enough? [v1 v2] (< (abs (- v1 v2)) tolerance))]
      (letfn [(try_ [guess]
                (let [next (f guess)]
                  (if (close-enough? guess next)
                    next
                    (try_ next))))]
        (try_ first-guess)))))

(println (fixed-point (fn [x] (Math/cos x)) 1.0))

; Newton's method
(defn deriv
  [g]
  (let [dx 0.00001]
    (fn [x]
      (/
        (- (g (+ x dx))
           (g x))
        dx))))

(println ((deriv cube) 5))

(defn newton-transform
  [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newton-method
  [g guess]
  (fixed-point (newton-transform g) guess))

(defn square [x] (* x x))

(defn sqrt
  [x]
  (newton-method (fn [y] (- (square y) x))
                 1.0))

(println (sqrt 4))

; exercise 1.40

(defn cubic
  [a b c]
  (fn [x]
    (+
      (cube x)
      (* a (square x))
      (* b x)
      c)))

(def x
  (let [a 1
        b 2
        c 3]
    (newton-method (cubic a b c) 1)))

(println ((cubic 1 2 3) x))

; exercise 1.41
(defn double_
  "Takes a procedure of one argument as argument and returns a procedure that applies the original procedure twice."
  [f]
  (fn [x]
    (f (f x))))

(println
  ((double_ inc) 0))

(println
  (((double_ double_) inc) 0))

(println (((double_ (double_ double_)) inc) 0))

;1
;(double (double ((double (double x))))) -> returns 16
;(double (double x)) -> returns 4

; exercise 1.42
(defn compose
  [f g]
  (fn [x]
    (f (g x))))

(assert (= 49 ((compose square inc) 6)))

; exercise 1.43
; assume n > 0
(defn repeated
  [f n]
  (fn [x]
    (loop [i 1
           result (f x)]
      (if (= i n)
        result
        (recur (inc i) (f result))))))

(println ((repeated square 2) 5))
(assert (= 625 ((repeated square 2) 5)))

; exercise 1.44

(defn average-smooth
  [f x]
  (let [dx 0.00001]
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
     3)))

(defn smooth
  [f]
  (fn [x]
    (average-smooth f x)))

(println ((smooth cube) 3))

(defn n-fold-smooth
  [f n]
  ((repeated smooth n) f))

(println ((n-fold-smooth cube 5) 3))

; exercise 1.46
(defn iterative-improvement
  [good-enough? improve]
  (fn [guess x]
    (loop [guess guess
           x x]
      (if (good-enough? guess x)
        guess
        (recur (improve guess x) x)))))
(defn improve
  [guess x]
  (average guess (/ x guess)))

(defn good-enough?
  [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn new-sqrt
  [x]
  ((iterative-improvement good-enough? improve) 1. x))

(println (new-sqrt 4))