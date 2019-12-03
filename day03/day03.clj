(require '[clojure.string :as str])
(require '[clojure.set :as set])
(def ->direction
  {
   "R" [0 1] "L" [0 -1] "U" [1 0] "D" [-1 0]
   })

(defn parse-move [move]
  (let [[_ d n] (re-find #"([RLUD])(\d+)" move)]
    [(->direction d) (Integer/parseInt n)]))

(defn move [start [direction n]]
  (->> (repeat n direction)
       (reductions #(mapv + %2 %1) start)
       (drop 1)))

(defn reducer [{:keys [pos] :as acc} movement]
  (let [movements (move pos movement)]
    (-> acc
        (assoc :pos (last movements))
        (update :positions concat movements))))

(defn follow-wire [wire]
  (->> (str/split wire #",")
       (map parse-move)
       (reduce reducer { :pos [0 0] :positions '() })
       :positions))

(defn manhattan-distance [a b]
  "d = |xa - xb| + |ya - yb| + |za - zb|"
  (let [abs-sub #(Math/abs (- %1 %2))]
    (reduce + (map abs-sub a b))))

(defn solve [wire1 wire2]
  (let [pos1 (set (follow-wire wire1))
        pos2 (set (follow-wire wire2))]
    (->> (set/intersection pos1 pos2)
         (map (partial manhattan-distance [0 0]))
         (reduce min))))

(solve "R8,U5,L5,D3" "U7,R6,D4,L4")

(def input (slurp "day03/input"))

(let [[a b] (str/split input #"\n")]
  (solve a b))

(defn wire-distance [pos positions]
  (let [l (count (take-while #(not= pos %) positions))]
    (if (= l (count positions))
      (Integer/MAX_VALUE)
      (inc l))))

(defn solve2 [wire1 wire2]
  (let [pos1 (follow-wire wire1)
        pos2 (follow-wire wire2)
        intersections (set/intersection (set pos1) (set pos2))]
    (->> intersections
         (map (fn [i] (+ (wire-distance i pos1) (wire-distance i pos2))))
         (reduce min))))

(assert (= 30 (solve2 "R8,U5,L5,D3" "U7,R6,D4,L4")))
(assert (= 610 (solve2 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83")))
(assert (= 410 (solve2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))

(let [[a b] (str/split input #"\n")]
  (solve2 a b))
