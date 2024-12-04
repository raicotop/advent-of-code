(ns day04)

(defn sol
  [in]
  (let [columns (->> (count in)
                     (range)
                     (map (fn [x] (map #(get % x) in)))
                     (map #(apply str %)))
        columns-rev (->> (map reverse columns)
                         (map #(apply str %)))
        rows-rev (->> (map reverse in)
                      (map #(apply str %)))
        diagonals-lr (->> (range (- (count in)) (inc (count in)))
                          (map (fn [x] (->> (range (count in))
                                            (map #(vector % (+ x %))))))
                          (map #(map (fn [[a b]] (get (get in a) b)) %))
                          (map #(apply str %)))
        diagonals-lr-rev (->> (map reverse diagonals-lr)
                              (map #(apply str %)))
        diagonals-rl (->> (range (* 2 (count in)))
                          (map (fn [x] (->> (range (count in))
                                            (map #(vector % (- x %))))))
                          (map #(map (fn [[a b]] (get (get in a) b)) %))
                          (map #(apply str %)))
        diagonals-rl-rev (->> (map reverse diagonals-rl)
                              (map #(apply str %)))]
    (->>  [in rows-rev
           columns columns-rev
           diagonals-lr diagonals-lr-rev
           diagonals-rl diagonals-rl-rev]
          (flatten)
          (map #(re-seq #"XMAS" %))
          (filter seq)
          (flatten)
          (count))))

(defn sol2
  [in]
  (let [squares (->> (for [x (range (- (count in) 2))
                           y (range (- (count in) 2))]
                       [x y])
                     (map (fn [[x y]]
                            (->> (for [dx (range 3)
                                       dy (range 3)]
                                   [(+ x dx) (+ y dy)])
                                 (map (fn [[x y]]
                                        (get (get in x) y))))))
                     (map #(apply str %)))
        is-x-mas? (fn [s] (or (re-seq #"M.S.A.M.S" s)
                              (re-seq #"S.M.A.S.M" s)
                              (re-seq #"M.M.A.S.S" s)
                              (re-seq #"S.S.A.M.M" s)))]
    (->> (filter is-x-mas? squares)
         (count))))

(defn process-input
  [file]
  (->> (slurp file)
       (clojure.string/split-lines)))

(comment
  (process-input "2024/day04demo.txt")
  (process-input "2024/day04.txt")
  (sol (process-input "2024/day04demo.txt")) ;=> 18
  (sol (process-input "2024/day04.txt")) ;=> 2297
  (sol2 (process-input "2024/day04demo.txt")) ;=> 31 
  (sol2 (process-input "2024/day04.txt")) ;=> 27267728
  )