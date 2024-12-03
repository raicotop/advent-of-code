(ns day03)

(defn sol
  [in]
  (->> (re-seq #"mul\(\d+,\d+\)" in)
       (map #(re-seq #"\d+" %))
       (map #(map (fn [x] (Integer/parseInt x)) %))
       (map (fn [[a b]] (* a b)))
       (reduce +)))

(defn sol2-filter
  [enabled? acc in]
  (if (empty? in)
    acc
    (let [op (first in)
          rest (rest in)]
      (cond
        (and (not enabled?) (= op "do()"))
        (sol2-filter true acc rest)
        (and enabled? (= op "don't()"))
        (sol2-filter false acc rest)
        (and enabled? (clojure.string/starts-with? op "mul"))
        (sol2-filter enabled? (conj acc op) rest)
        :else (sol2-filter enabled? acc rest)))))

(defn sol2
  [in]
  (->> (re-seq #"mul\(\d+,\d+\)|do\(\)|don\'t\(\)" in)
       (sol2-filter true [])
       (map #(re-seq #"\d+" %))
       (map #(map (fn [x] (Integer/parseInt x)) %))
       (map (fn [[a b]] (* a b)))
       (reduce +)))

(defn process-input
  [file]
  (slurp file))

(comment
  (process-input "2024/day03demo.txt")
  (process-input "2024/day03.txt")
  (sol (process-input "2024/day03demo.txt")) ;=> 161
  (sol (process-input "2024/day03.txt")) ;=> 173529487
  (sol2 (process-input "2024/day03demo2.txt")) ;=> 48 
  (sol2 (process-input "2024/day03.txt")) ;=> 99532691
  )