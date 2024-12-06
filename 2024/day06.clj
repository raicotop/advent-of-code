(ns day06)

(def directions
  [[-1 0] [0 1] [1 0] [0 -1]])

(defn sol
  [in]
  (loop [curr-pos (->> (for [y (range (count in))
                             x (range (count (first in)))
                             :when (= (get-in in [x y]) \^)]
                         [x y])
                       (first))
         directions-cycle (cycle directions)
         acc #{}]
    (let [current-direction (first directions-cycle)
          next-pos [(+ (first curr-pos) (first current-direction))
                    (+ (second curr-pos) (second current-direction))]
          acc-new (conj acc curr-pos)]
      (case (get-in in next-pos)
        nil (count (conj acc curr-pos))
        \# (recur [(+ (first curr-pos) (first (second directions-cycle)))
                   (+ (second curr-pos) (second (second directions-cycle)))]
                  (rest directions-cycle) acc-new)
        (recur next-pos directions-cycle acc-new)))))

(defn possible-obstructions
  [in]
  (let [first-position (->> (for [y (range (count in))
                                  x (range (count (first in)))
                                  :when (= (get-in in [x y]) \^)]
                              [x y])
                            (first))]
    (loop [curr-pos first-position
           directions-cycle (cycle directions)
           acc #{}]
      (let [current-direction (first directions-cycle)
            next-pos [(+ (first curr-pos) (first current-direction))
                      (+ (second curr-pos) (second current-direction))]
            acc-new (conj acc curr-pos)]
        (case (get-in in next-pos)
          nil (disj (conj acc curr-pos) first-position)
          \# (recur [(+ (first curr-pos) (first (second directions-cycle)))
                     (+ (second curr-pos) (second (second directions-cycle)))]
                    (rest directions-cycle) acc-new)
          (recur next-pos directions-cycle acc-new))))))

(defn cycle?
  [in]
  (let [first-position (->> (for [y (range (count in))
                                  x (range (count (first in)))
                                  :when (= (get-in in [x y]) \^)]
                              [x y])
                            (first))]
    (loop [curr-pos first-position
           directions-cycle (cycle directions)
           acc #{}]
      (let [current-direction (first directions-cycle)
            next-pos [(+ (first curr-pos) (first current-direction))
                      (+ (second curr-pos) (second current-direction))]
            acc-new (conj acc [curr-pos current-direction])]
        (if (contains? acc [curr-pos current-direction])
          true
          (case (get-in in next-pos)
            nil false
            \# (recur curr-pos (rest directions-cycle) acc-new)
            (recur next-pos directions-cycle acc-new)))))))

(defn sol2
  [in]
  (->> (possible-obstructions in)
       (map (fn [pos]
              (assoc-in (vec (map vec in)) pos \#)))
       (filter cycle?)
       (count)))

(defn process-input
  [file]
  (->> (slurp file)
       (clojure.string/split-lines)))

(comment
  (sol (process-input "2024/day06demo.txt")) ;=> 41
  (sol (process-input "2024/day06.txt")) ;=> 5409
  (sol2 (process-input "2024/day06demo.txt")) ;=> 6 
  (sol2 (process-input "2024/day06.txt")) ;=> 2022
  )