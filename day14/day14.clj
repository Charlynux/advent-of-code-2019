(require '[clojure.string :as str])

(defn parse-line [line]
  (let [[elements product] (str/split line #" => ")]
    (->> (conj
          (str/split elements #", ")
          product)
         (map #(str/split % #" "))
         (map (fn [[n id]] [(Integer/parseInt n) id])))))

(parse-line "1 GWZRW => 9 CHNX")
(parse-line "3 NPNGZ, 3 TBFQ, 1 RZBF, 4 OIOIH => 2 LQNR")

;; Exploring input data
(def input (slurp "day14/input"))

;; Check if all "elements" could be produce by only one combination
(->> (str/split input #"\n")
     (map #(last (str/split % #" ")))
     (distinct?))

(defn simple-index [elements]
  ;; simplify problem by removing coefficients
  (let [elts (map second elements)]
    [(last elts) (butlast elts)]))

(defn parse-input [index-fn input]
  (into {}
        (comp
         (map parse-line)
         (map index-fn))
        (str/split input #"\n")))

;; Check if path from FUEL to ORE is OK
(let [elements (parse-input simple-index input)
      branch? #(contains? elements %)
      children elements]
  (tree-seq branch? children "FUEL"))

(defn index-elements [elements]
  (let [product (last elements)]
    [(second product)
     {:children (butlast elements)
      :quantity (first product)}]))

(defn needed-quantity [wanted productible]
  (if (> productible wanted)
    1
    (quot wanted productible)))

(= (needed-quantity 4 2) 2)
(= (needed-quantity 3 9) 1)

(defn solve [input]
  (let [elements (parse-input index-elements input)
        branch? #(contains? elements %)
        children (comp :children elements)
        walk (fn walk [wanted node]
               (let [coeff (needed-quantity wanted (or (some-> node elements :quantity)
                                                       1))]
                 (if (branch? node)
                   (reduce + (map #(walk (* coeff (first %))
                                         (second %)) (children node)))
                   wanted)))]
    (walk 1 "FUEL")))

(solve "1 ORE => 1 AB
1 ORE => 1 BC
3 ORE => 2 CA
2 AB, 3 BC, 4 CA => 1 FUEL")

(= 31 (solve "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL"))

(= 165 (solve "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL"))
