(ns day01)

(defn sol
  [in]
  (->> (map sort in)
       (reduce interleave)
       (partition 2)
       (map (fn [[a b]] (Math/abs (- a b))))
       (reduce +)))

(defn sol2
  [in]
  (let [a (first in)
        b (frequencies (second in))]
    (->> (map #(vector % (get b % 0)) a)
         (map #(reduce * %))
         (reduce +))))

(defn process-input
  [file]
  (->> (slurp file)
       (clojure.string/split-lines)
       (map #(re-seq #"\d+" %))
       (map #(map (fn [x] (Integer/parseInt x)) %))
       (#(vector (map first %) (map second %)))))

(comment
  (process-input "2024/day01demo.txt")
  (process-input "2024/day01.txt")
  (sol (process-input "2024/day01demo.txt")) ;=> 11
  (sol (process-input "2024/day01.txt")) ;=> 1319616
  (sol2 (process-input "2024/day01demo.txt")) ;=> 31 
  (sol2 (process-input "2024/day01.txt")) ;=> 27267728
  )