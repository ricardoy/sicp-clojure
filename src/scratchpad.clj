(ns scratchpad)

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


