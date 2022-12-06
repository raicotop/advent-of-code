(ns day06)

(defn sol
  [n]
  (->> (slurp "2022/day06.txt")
       (partition n 1)
       (map set)
       (map count)
       (interleave (range))
       (partition 2)
       (filter (fn [[a b]] (= b n)))
       (first)
       (first)
       (+ n)))

(comment
  (sol 4) ;=> 1987
  (sol 14) ;=> 3059
  )