(ns day03)

(defn score
  [c]
  (let [base (int c)
        lowercase? (fn [c] (>= (int c) 97))
        base-off (if (lowercase? c) 96 38)]
    (- base base-off)))

(defn sol1
  [in]
  (->> (map #(split-at (/ (count %) 2) %) in)
       (map (fn [[a b]] (clojure.set/intersection (set a) (set b))))))

(defn sol2
  [in]
  (->> (map frequencies in)
       (map keys)
       (partition 3)
       (map #(reduce clojure.set/intersection (map set %)))))

(defn sol
  [f]
  (->> (slurp "2022/day03.txt")
       (clojure.string/split-lines)
       (f)
       (map #((comp score first) %))
       (reduce +)))

(comment
  (sol sol1) ; => 7878
  (sol sol2) ; => 2760
  )