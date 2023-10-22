(ns chapter2_1)

(defn gcd
  [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))

(defn make-rat-original
  "creates a ration number given a numerator and a denominator"
  [n d]
  (cons n [d]))
(defn make-rat-unique
  "creates a ration number given a numerator and a denominator"
  [n d]
  (let [g (gcd n d)]
    (cons (/ n g)  [(/ d g)])))

(defn make-rat
  "creates a ration number given a numerator and a denominator"
  [n d]
  (let [g (gcd n d)
        sign (if (< (* n d) 0) -1 1)]
    (cons (* sign (abs (/ n g)))
          [(abs (/ d g))])))

(defn numer
  "returns the numerator of the rational number x"
  [x]
  (first x))


(defn denom
  "returns the denominator of the rational number x"
  [x]
  (last x))


(defn add-rat
  [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat
  [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat
  [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat
  [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat?
  [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn print-rat
  [x]
  (println)
  (print (numer x))
  (print "/")
  (print (denom x)))

(def one-half (make-rat 1 2))

(print-rat one-half)

(let [x (make-rat 4 8)]
  (assert (= 1 (numer x)))
  (assert (= 2 (denom x))))

(let [x (make-rat -4 -8)]
  (assert (= 1 (numer x)))
  (assert (= 2 (denom x))))

(let [x (make-rat -4 8)]
  (assert (= -1 (numer x)))
  (assert (= 2 (denom x))))

(let [x (make-rat 4 -8)]
  (assert (= -1 (numer x)))
  (assert (= 2 (denom x))))

; exercise 2.2

(defn make-point
  [x y]
  [x y])

(defn x-point
  [point]
  (first point))

(defn y-point
  [point]
  (last point))

(defn make-segment
  [start-segment
   end-segment]
  [start-segment end-segment])

(defn start-segment
  [s]
  (first s))

(defn end-segment
  [s]
  (last s))

(defn print-point
  [p]
  (println)
  (print "(")
  (print (x-point p))
  (print ",")
  (print (y-point p))
  (print ")")
  (println))

(let [p (make-point
          (make-rat 1 1)
          (make-rat 2 1))]
  (assert (= 1 (numer (x-point p))))
  (assert (= 1 (denom (x-point p))))
  (assert (= 2 (numer (y-point p))))
  (assert (= 1 (denom (y-point p))))
  (print-point p))

(defn average
  [x y]
  (div-rat
    (add-rat x y)
    (make-rat 2 1)))

(defn midpoint-segment
  [line-segment]
  (make-point
    (average
      (x-point (start-segment line-segment))
      (x-point (end-segment line-segment)))
    (average
      (y-point (start-segment line-segment))
      (y-point (end-segment line-segment)))))

(let [p1 (make-point (make-rat 0 1) (make-rat 0 1))
      p2 (make-point (make-rat 2 1) (make-rat 4 1))
      s (make-segment p1 p2)]
  (assert (= 0 (numer (x-point (start-segment s))))))

(let [segment (make-segment
                (make-point (make-rat 0 1) (make-rat 0 1))
                (make-point (make-rat 2 1) (make-rat 4 1)))
      midpoint (midpoint-segment segment)]
  (assert (= 1 (numer (x-point midpoint))))
  (assert (= 2 (numer (y-point midpoint)))))
