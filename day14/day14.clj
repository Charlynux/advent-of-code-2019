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
         (map str/trim)
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

(defn calculate-ratio [wanted productible]
  (if (<= wanted 0)
    0
    (if (> productible wanted)
      1
      (int (Math/ceil (float (/ wanted productible)))))))

(= (calculate-ratio 4 2) 2)
(= (calculate-ratio 3 9) 1)
(= (calculate-ratio 13 3) 5)
(zero? (calculate-ratio -2 10))

(defn solve [input]
  (let [elements (parse-input index-elements input)
        stock (atom {})
        branch? #(contains? elements (second %))
        children (fn [[requested chemical]]
                   #_(println @stock)
                   (let [{:keys [quantity children]} (elements chemical)
                         wanted (- requested (get @stock chemical 0))
                         ratio (calculate-ratio wanted quantity)]
                     (swap! stock #(assoc % chemical (- (* quantity ratio) wanted)))
                     (map (fn [[q n]] [(* ratio q) n]) children)))]
    (->> (tree-seq branch? children [1 "FUEL"])
         (filter (comp #{"ORE"} second))
         (map first)
         (reduce +))))

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

(def example-13312
  "157 ORE => 5 NZVS
  165 ORE => 6 DCFZ
  44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
  12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
  179 ORE => 7 PSHF
  177 ORE => 5 HKGWZ
  7 DCFZ, 7 PSHF => 2 XJWVT
  165 ORE => 2 GPVTF
  3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")

(= 13312 (solve example-13312))

(def example-180697
  "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
  17 NVRVD, 3 JNWZP => 8 VPVL
  53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
  22 VJHF, 37 MNCFX => 5 FWMGM
  139 ORE => 4 NVRVD
  144 ORE => 7 JNWZP
  5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
  5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
  145 ORE => 6 MNCFX
  1 NVRVD => 8 CXFTF
  1 VJHF, 6 MNCFX => 4 RFSQX
  176 ORE => 6 VJHF")
(= 180697 (solve example-180697))

(def example-2210736
  "171 ORE => 8 CNZTR
  7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
  114 ORE => 4 BHXH
  14 VRPVC => 6 BMBT
  6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
  6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
  15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
  13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
  5 BMBT => 4 WPTQ
  189 ORE => 9 KTJDG
  1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
  12 VRPVC, 27 CNZTR => 2 XDBXC
  15 KTJDG, 12 BHXH => 5 XCVML
  3 BHXH, 2 VRPVC => 7 MZWV
  121 ORE => 7 VRPVC
  7 XCVML => 6 RJRHP
  5 BHXH, 4 VRPVC => 5 LTCX")

(= 2210736 (solve example-2210736))

(solve input)

;; PART 2 - WORK IN PROGRESS

(defn iteration [elements]
  (fn [stock-in]
    (let [stock (atom stock-in)
          branch? (constantly true)
          children (fn [[requested chemical]]
                     #_(println @stock)
                     (let [{:keys [quantity children]} (elements chemical { :quantity 0 :children []})
                           wanted (- requested (get @stock chemical 0))
                           ratio (calculate-ratio wanted quantity)]
                       (swap! stock #(assoc % chemical (- (* quantity ratio) wanted)))
                       (map (fn [[q n]] [(* ratio q) n]) children)))]
      (doall (tree-seq branch? children [1 "FUEL"]))
      @stock)))

(defn solve2 [input]
  (let [elements (parse-input index-elements input)]
    (->> (iterate (iteration elements) { "ORE" 1000000000000 })
         (drop 1) ;; Skip initial state
         ;; Cycle
         (drop-while (fn [stock] (some pos? (vals (dissoc stock "ORE")))))
         first)))

(/ (- 1000000000000 999939198545) 1000000000000)
;; Cycle after 5039 - ORE 999939198545

(= 82892753 (solve2 example-13312))
(= 5586022 (solve2 example-180697))
(= 460664 (solve2 example-2210736))

#_(solve2 input)
