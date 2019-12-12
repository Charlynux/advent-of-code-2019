(def test-moons
  [[-1 0 2]
   [2 -10 -7]
   [4 -8 8]
   [3 5 -1]])

(def moons
  [[8 0 8]
   [0 -5 -10]
   [16 10 -5]
   [19 -10 -7]])

(defn init-moons [moons]
  (map (fn [moon] (hash-map :position moon :velocity [0 0 0])) moons))

(defn update-val [a b] (cond (= a b) 0
                             (< a b) +1
                             (> a b) -1))

(defn update-velocity [moons]
  (map
   (fn [moon]
     (let [updates (for [other moons
                         :when (not= other moon)]
                     (mapv update-val (:position moon) (:position other)))]
       (update moon :velocity (fn [v] (reduce #(mapv + %1 %2) v updates)))))
   moons))

(comment
  (update-velocity
   (init-moons test-moons)))

(defn move-moon [moon]
  (update moon :position (fn [p] (mapv + p (:velocity moon)))))

(comment
  (move-moon {:position [-1 0 2] :velocity [3 -1 -1]}))

(defn iteration [moons]
  (->> moons
       update-velocity
       (map move-moon)))

(comment
  (take 10 (iterate iteration (init-moons test-moons)))

  (nth (iterate iteration (init-moons test-moons)) 10))

(defn potential-energy [moon] (reduce + (map #(Math/abs %) (:position moon))))
(defn kinetic-energy [moon] (reduce + (map #(Math/abs %) (:velocity moon))))

(defn total-energy [moon] (* (potential-energy moon) (kinetic-energy moon)))

(comment
  (map total-energy
       (nth (iterate iteration (init-moons test-moons)) 10))
  )

(defn solve [moons]
  (->> moons
       init-moons
       (iterate iteration)
       (#(nth % 1000))
       (map total-energy)
       (reduce + 0)))

(solve moons)


(defn solve2 [moons]
  (->> moons
       init-moons
       (iterate iteration)
       (reduce (fn [acc val] (let [h (hash val)]
                              (if (acc h) (reduced acc) (conj acc h)))) #{})
       count))

(solve2 test-moons)
(solve2 [[-8 -10 0] [5 5 10] [2 -7 3] [9 -8 -3]])
(solve2 moons) ;; your answer is too low | 118921
