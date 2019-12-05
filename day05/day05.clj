(defmulti proceed (fn [instruction & args] instruction))
(defmethod proceed 1 [_ index values]
  (let [pos1 (+ index 1)
        pos2 (+ index 2)
        pos-result (+ index 3)
        a (get values pos1)
        b (get values pos2)]
    [(+ index 4) (assoc values pos-result (+ a b))]))
(defmethod proceed 2 [_ index values]
  (let [pos1 (+ index 1)
        pos2 (+ index 2)
        pos-result (+ index 3)
        a (get values pos1)
        b (get values pos2)]
    [(+ index 4) (assoc values pos-result (* a b))]))

(defn execute [values]
  (loop [index 0 values values]
    (let [instruction (get values index)]
      (if (= 99 instruction)
        values
        (let [[next-index next-values] (proceed instruction index values)]
          (recur next-index next-values))))))

(defn read-input [file] (read-string (str "[" (slurp file) "]")))

(assert (= 3654868 (-> (read-input "day02/input") (assoc 1 12 2 2) execute first)))
(assert (= 19690720 (-> (read-input "day02/input") (assoc 1 70 2 14) execute first)))
