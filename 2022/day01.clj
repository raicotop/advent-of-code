(ns day01)

(defn sol
  [in]
  (reduce max in))

(defn sol2
  [in]
  (->> (sort > in)
       (take 3)
       (reduce +)))

(defn process-input
  [file]
  (->> (slurp file)
       (clojure.string/split-lines)
       (partition-by #{""})
       (remove #{[""]})
       (map #(map (fn [x] (Integer/parseInt x)) %))
       (map #(reduce + %))))

(comment
  (sol (process-input "2022/day01.txt")) ;=> 74394
  (sol2 (process-input "2022/day01.txt")) ;=> 212836
  )