(require '[intcode :as intcode])

(def result (intcode/execute (intcode/read-input-file "day17/input")))

(def camera-view (->> result
                      :outputs
                      (map char)
                      (partition-by #{\newline})
                      (remove #(= 1 (count %)))))

(defn index-line [y line]
  (map-indexed (fn [x v]
                 [[x y] v]) line))

(def grid->coords (comp (map-indexed index-line) (mapcat identity)))

(def indexed-view (into {} grid->coords camera-view))
(def scaffolds (into [] (comp grid->coords (filter (comp #{\#} second)) (map first)) camera-view))

(defn neighbors [position]
  (map #(mapv + position %) [[0 1] [0 -1] [1 0] [-1 0]]))

(def count-scaffolds-neighbors
  (comp count #(filter #{\#} %) #(map indexed-view %) neighbors))

(->> scaffolds
     (filter #(= 4 (count-scaffolds-neighbors %)))
     (map #(apply * %))
     (reduce +))
