(require '[intcode :as intcode])

(defn apply-coords [program coords]
  (-> program
      (dissoc :halted)
      (assoc :inputs coords :outputs [])
      intcode/run))

(defn read-status [program] (get-in program [:outputs 0]))

(defn iteration [program]
  (fn [board coords]
    (let [status (read-status (apply-coords program coords))]
      (assoc board coords status))))

(defn generate-coords []
  (for [x (range 50)
        y (range 50)]
    [x y]))

(def grid (reduce
           (iteration (intcode/init-program (intcode/read-input-file "day19/input")))
           {}
           (generate-coords)))

(def ->point
  {0 "."
   1 "#"
   -1 "?"})

(defn draw-grid [grid]
  (doseq [y (range 50)]
    (doseq [x (range 50)]
      (print (-> [x y] grid ->point)))
    (println "")))

(draw-grid grid)


(defn solve []
  (->> (reduce
        (iteration (intcode/init-program (intcode/read-input-file "day19/input")))
        {}
        (generate-coords))
       vals
       (filter #{1})
       count))

(time (solve))

(defn simpler-part1 []
  (let [program (intcode/init-program (intcode/read-input-file "day19/input"))]
    (transduce
     (comp
      (map #(apply-coords program %))
      (map read-status)
      (filter #{1}))
     (fn
       ([] 0)
       ([res] res)
       ([res _] (inc res)))
     (generate-coords))))

(time (simpler-part1))
