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
