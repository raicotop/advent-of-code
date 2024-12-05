(ns day05)

(defn sol
  [[rules pages-list]]
  (let [pages-map (map #(zipmap % (range)) pages-list)
        valid? (fn [pages]
                 (->> rules
                      (map (fn [rule]
                             (let [[a b] rule]
                               (or (not (contains? pages a))
                                   (not (contains? pages b))
                                   (and (contains? pages a)
                                        (contains? pages b)
                                        (<= (get pages a) (get pages b)))))))
                      (every? true?)))]
    (->> (filter valid? pages-map)
         (map (fn [pages] (filter (fn [[k v]] (= v (/ (dec (count pages)) 2))) pages)))
         (map (fn [pages] (map first pages)))
         (flatten)
         (reduce +))))

(defn topological-order
  [in subset]
  (loop [rules (->> (filter (fn [[a b]] (and (contains? (set subset) a) (contains? (set subset) b))) in))
         acc (list)
         to-process (->> (flatten rules)
                         (set))
         max-cycle 500000]
    (if (or (empty? rules)
            (= 1 (count to-process))
            (zero? max-cycle))
      (concat to-process acc)
      (let [left-rules (->> rules
                            (map first)
                            (set))
            rightest-rule (->> rules
                               (map second)
                               (remove (fn [rule]
                                         (contains? left-rules rule)))
                               (first))
            rules-new (remove #(= (second %) rightest-rule) rules)
            acc-new (conj acc rightest-rule)
            to-process-new (disj to-process rightest-rule)]
        (recur rules-new acc-new to-process-new (dec max-cycle))))))

(defn sol2
  [[rules pages-list]]
  (let [pages-map (map #(zipmap % (range)) pages-list)
        valid? (fn [pages]
                 (->> rules
                      (map (fn [rule]
                             (let [[a b] rule]
                               (or (not (contains? pages a))
                                   (not (contains? pages b))
                                   (and (contains? pages a)
                                        (contains? pages b)
                                        (<= (get pages a) (get pages b)))))))
                      (every? true?)))]
    (->> (remove valid? pages-map)
         (map (fn [pages] (topological-order rules (keys pages))))
         (map (fn [pages] (nth pages (/ (dec (count pages)) 2))))
         (reduce +))))

(defn process-input
  [file]
  (let [[rules pages] (->> (slurp file)
                           (clojure.string/split-lines)
                           (split-with #(not= % "")))
        rules-processed (->> (map (fn [x] (clojure.string/split x #"\|")) rules)
                             (map #(map (fn [a] (Integer/parseInt a)) %)))
        pages-processed (->> (rest pages)
                             (map #(clojure.string/split % #","))
                             (map (fn [x] (map #(Integer/parseInt %) x))))]
    [rules-processed pages-processed]))

(comment
  (process-input "2024/day05demo.txt")
  (process-input "2024/day05.txt")
  (sol (process-input "2024/day05demo.txt")) ;=> 143
  (sol (process-input "2024/day05.txt")) ;=> 4569
  (sol2 (process-input "2024/day05demo.txt")) ;=> 123 
  (sol2 (process-input "2024/day05.txt")) ;=> 6456
  )