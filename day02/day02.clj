(require '[intcode :refer [execute read-input-file]])

(defn solve-generic [noun verb values]
  (-> values
      (assoc 1 noun 2 verb)
      execute
      :values
      first))

(def input (read-input-file "day02/input"))

(solve 12 2 input)

(defn find-value [stop-value values]
  (let [stop? (fn [[_ _ value]] (not= value stop-value))
        solutions (for [noun (range 0 100) verb (range 0 100)]
                    [noun verb (solve-generic noun verb input)])]
    (->> solutions (drop-while stop?) first)))

(defn solve-2 [searched-value values]
  (let [[noun verb _] (find-value searched-value values)]
    (+ (* 100 noun) verb)))

(solve-2 19690720 input)
