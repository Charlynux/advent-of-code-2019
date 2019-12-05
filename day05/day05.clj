(defn position-mode? [instruction offset]
  (zero? (mod (quot instruction (Math/pow 10 (inc offset))) 10)))

(defn read-mode [instruction offset]
  (if (position-mode? instruction offset)
    (fn [index values] (->> index (+ offset) (get values) (get values)))
    (fn [index values] (->> index (+ offset) (get values)))))

(comment
  ((read-mode 1002 1) 0 [1002,4,3,4,33])
  ((read-mode 1002 2) 0 [1002,4,3,4,33]))

(defn write-mode [instruction offset]
  (if (position-mode? instruction offset)
    (fn [index value values] (assoc values (get values (+ index offset)) value))
    (fn [index value values] (assoc values (+ index offset) value))))

(def ^:dynamic INPUT-VALUE 1)

(def proceed nil)
(defmulti proceed (fn [instruction & args] (mod instruction 100)))
(defmethod proceed 1 [instruction index values]
  (let [a ((read-mode instruction 1) index values)
        b ((read-mode instruction 2) index values)]
    [(+ index 4) ((write-mode instruction 3) index (+ a b) values)]))
(defmethod proceed 2 [instruction index values]
  (let [a ((read-mode instruction 1) index values)
        b ((read-mode instruction 2) index values)]
    [(+ index 4) ((write-mode instruction 3) index (* a b) values)]))
(defmethod proceed 3 [instruction index values]
  [(+ index 2) ((write-mode instruction 1) index INPUT-VALUE values)])
(defmethod proceed 4 [instruction index values]
  (println ((read-mode instruction 1) index values))
  [(+ index 2) values])
(defmethod proceed 5 [instruction index values]
  (let [next (if (zero? ((read-mode instruction 1) index values))
               (+ index 3)
               ((read-mode instruction 2) index values))]
    [next values]))
(defmethod proceed 6 [instruction index values]
  (let [next (if (zero? ((read-mode instruction 1) index values))
               ((read-mode instruction 2) index values)
               (+ index 3))]
    [next values]))
(defmethod proceed 7 [instruction index values]
  (let [value (if (< ((read-mode instruction 1) index values)
                     ((read-mode instruction 2) index values))
                1
                0)]
    [(+ index 4) ((write-mode instruction 3) index value values)]))
(defmethod proceed 8 [instruction index values]
  (let [value (if (= ((read-mode instruction 1) index values)
                     ((read-mode instruction 2) index values))
                1
                0)]
    [(+ index 4) ((write-mode instruction 3) index value values)]))


(defn execute [values]
  (loop [index 0 values values]
    (let [instruction (get values index)]
      (if (= 99 instruction)
        values
        (let [[next-index next-values] (proceed instruction index values)]
          (recur next-index next-values))))))

(assert (= 3500 (first (execute [1,9,10,3,2,3,11,0,99,30,40,50]))))
(assert (= 1002 (first (execute [1002,4,3,4,33]))))
(assert (= 1101 (first (execute [1101,100,-1,4,0]))))

(comment
  (execute [3,0,4,0,99])
  (execute [3,9,8,9,10,9,4,9,99,-1,8])
  (execute [3,9,7,9,10,9,4,9,99,-1,8])
  (execute [3,3,1108,-1,8,3,4,3,99])
  (execute [3,3,1107,-1,8,3,4,3,99])
  (binding [INPUT-VALUE 8]
    (execute [3,9,8,9,10,9,4,9,99,-1,8])))

(defn read-input [file] (read-string (str "[" (slurp file) "]")))

(assert (= 3654868 (-> (read-input "day02/input") (assoc 1 12 2 2) execute first)))
(assert (= 19690720 (-> (read-input "day02/input") (assoc 1 70 2 14) execute first)))

(-> (read-input "day05/input") execute)

(binding [INPUT-VALUE 5]
  (-> (read-input "day05/input") execute))
