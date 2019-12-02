(def example [1,9,10,3,2,3,11,0,99,30,40,50])

(def opcodes { 1 +  2 * })

(defn apply-opcode [opcode pos1 pos2 pos-result values]
  (let [operation (get opcodes opcode)
        a (get values pos1)
        b (get values pos2)]
    (assoc values pos-result (operation a b))))

(defn execute [values]
  (loop [index 0 values values]
    (let [next-index (+ index 4)
          [opcode pos1 pos2 pos-result] (subvec values index next-index)]
      (if (= 99 opcode)
        values
        (recur next-index (apply-opcode opcode pos1 pos2 pos-result values))))))

(assert (= 3500 (first (execute example))))

(defn solve [values]
  (-> values
      (assoc 1 12 2 2) ;1202 program alarm
      execute
      first))

(def input (read-string (str "[" (slurp "input") "]")))

(solve input)

(defn solve-generic [noun verb values]
  (-> values
      (assoc 1 noun 2 verb)
      execute
      first))

(defn find-value [stop-value values]
  (let [stop? (fn [[_ _ value]] (not= value stop-value))
        solutions (for [noun (range 0 100) verb (range 0 100)]
                    [noun verb (solve-generic noun verb input)])]
    (->> solutions (drop-while stop?) first)))

(defn solve-2 [searched-value values]
  (let [[noun verb _] (find-value searched-value values)]
    (+ (* 100 noun) verb)))

(solve-2 19690720 input)
