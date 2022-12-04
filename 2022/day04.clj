(ns day04)

(defn sol
  [f]
  (->> (slurp "2022/day04.txt")
       (clojure.string/split-lines)
       (map #(clojure.string/split % #","))
       (map #(map (fn [x] (clojure.string/split x #"-")) %))
       (map #(map (fn [x] (map (fn [y] (Integer/parseInt y)) x)) %))
       (map #(map (fn [[a b]] (set (range a (inc b)))) %))
       (map f)
       (map #(if % 1 0))
       (reduce +)))

(comment
  (sol (fn [[a b]] (= (count (clojure.set/intersection a b))
                      (min (count a) (count b)))))
  ;=> 441
  
  (sol (fn [[a b]] (> (count (clojure.set/intersection a b))
                      0)))
  ;=> 861
  
  )