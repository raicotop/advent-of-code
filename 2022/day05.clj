(ns day05)

(defn parse-stacks
  [stacks]
  (->> stacks
       (map #(partition 3 4 %))
       (map #(map second %))
       ((fn [rows]
          (->> (for [y (range (count (first rows)))
                     x (range (count rows))]
                 (nth (nth rows x) y))
               (partition (count rows)))))
       (map reverse)
       (map #(remove #{\space \1 \2 \3 \4 \5 \6 \7 \8 \9} %))))

(defn parse-instructions
  [instructions]
  (->> instructions
       (map #(clojure.string/split % #" "))
       (map #(remove #{"move" "from" "to"} %))
       (map #(map (fn [x] (Integer/parseInt x)) %))))

(defn parse-inputs
  [[stacks instructions]]
  [(parse-stacks stacks) (parse-instructions instructions)])

(defn process-inputs
  [reverse? [stacks instructions]]
  (loop [s stacks
         i instructions]
    (if (empty? i)
      s
      (let [i' (first i)
            move (nth i' 0)
            from (dec (nth i' 1))
            to (dec (nth i' 2))
            crates (take-last move (nth s from))
            from-stack (drop-last move (nth s from))
            to-stack (concat (nth s to) (if reverse? (reverse crates) crates))
            new-stacks (-> (vec s)
                           (assoc from from-stack)
                           (assoc to to-stack))
            new-instructions (rest i)]
        (recur new-stacks new-instructions)))))

(defn sol
  [reverse?]
  (->> (slurp "2022/day05.txt")
       (clojure.string/split-lines)
       (partition-by #{""})
       (remove #{[""]})
       (parse-inputs)
       (process-inputs reverse?)
       (map last)
       (reduce str)))

(comment
  (sol true) ;=> "SHMSDGZVC"
  (sol false) ;=> "VRZGHDFBQ"
  )