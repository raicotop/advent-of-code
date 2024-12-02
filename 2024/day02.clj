(ns day02)

(defn sol
  [in]
  (let [diffs (fn [row] 
                (->> (interleave (drop-last row) (drop 1 row))
                     (partition 2)
                     (map (fn [[a b]] (- a b)))
                     (set)))
        monotonous? (fn [nums]
                      (or (every? pos? nums)
                          (every? neg? nums)))
        in-range? (fn [nums]
                    (->> (map #(Math/abs %) nums)
                         (every? #(and (<= 1 %) (<= % 3)))))]
    
    (->> (map diffs in)
         (filter #(and (monotonous? %) (in-range? %)))
         (count))))

;; ugly, but works
(defn sol2
  [in]
  (let [remove-at (fn [coll i]
                    (concat (take i coll) (drop (inc i) coll)))
        valid-row? (fn [row]
                     (->> (count row)
                          (range)
                          (map #(remove-at row %))
                          (map #(sol (vector %)))
                          (filter #(= 1 %))
                          (seq)))]
    (->> (filter valid-row? in)
       (count))))

(defn process-input
  [file]
  (->> (slurp file)
       (clojure.string/split-lines)
       (map #(re-seq #"\d+" %))
       (map #(map (fn [x] (Integer/parseInt x)) %))))

(comment
  (process-input "2024/day02demo.txt")
  (process-input "2024/day02.txt")
  (sol (process-input "2024/day02demo.txt")) ;=> 2
  (sol (process-input "2024/day02.txt")) ;=> 390
  (sol2 (process-input "2024/day02demo.txt")) ;=> 4 
  (sol2 (process-input "2024/day02.txt")) ;=> 439
  )