(def base-pattern [0, 1, 0, -1])

(defn indexed-pattern [n] (mapcat #(repeat n %) base-pattern))
(defn pattern [n] (drop 1 (cycle (indexed-pattern n))))

(assert (= [0 0 0 1 1 1 0 0 0 -1 -1 -1] (indexed-pattern 3)))

(defn to-digit [n] (mod (Math/abs n) 10))

(defn calculate-element [signal n]
  (->> signal
       (map * (pattern n))
       (reduce +)
       to-digit))

(assert (= 4 (calculate-element [1 2 3 4 5 6 7 8] 1)))
(assert (= 8 (calculate-element [1 2 3 4 5 6 7 8] 2)))

(defn phase [l]
  (fn [signal] (map
               #(calculate-element signal %)
               (range 1 (inc l)))))

(assert (= [4 8 2 2 6 1 5 8] ((phase 8) [1 2 3 4 5 6 7 8])))

(defn solve [input]
  (let [signal (map #(Integer/parseInt %) (clojure.string/split input #""))
        phases (iterate (phase (count signal)) signal)]
    (clojure.string/join (take 8 (nth phases 100)))))

(assert (= "24176176" (solve "80871224585914546619083218645595")))
(assert (= "73745418" (solve "19617804207202209144916044189917")))
(assert (= "52432133" (solve "69317163492948606335995924319873")))

(def input (slurp "day16/input"))

(solve input)
;; 15841929
