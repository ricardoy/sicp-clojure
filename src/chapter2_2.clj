(ns chapter2_2)

(defn augment-list [tuple]
  (map
    (fn [x] (cons x tuple))
    (range 1 (first tuple))))

;(print
;  (augment-list [5 4 3]))
;
(defn generate-n-tuples [dimension max-value]
  (loop [dimension (dec dimension)
         result (map (fn [x] [x])
                     (range 1 (inc max-value)))]
    (if (zero? dimension)
      result
      (recur (dec dimension)
             (reduce (fn [processed car]
                         (concat (augment-list car)
                               processed))
                     []
                     result)))))

(print (generate-n-tuples 4 6))
