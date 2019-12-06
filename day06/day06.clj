(def example
  "COM)B
  B)C
  C)D
  D)E
  E)F
  B)G
  G)H
  D)I
  E)J
  J)K
  K)L")

(defn parse-input [input]
  (->> (clojure.string/split input #"\n")
       (map clojure.string/trim)
       (map #(clojure.string/split % #"\)"))))

(comment
  (parse-input example))

(defn solve [input]
  (let [datas (parse-input input)
        links (reduce
               (fn [acc [k v]]
                 (update acc k (fnil (partial cons v) #{})))
               {}
               datas)
        branch? #(contains? links %)
        children links
        root (first (clojure.set/difference (set (map first datas)) (set (map second datas))))
        ;; Like tree-seq but with sum depth
        walk (fn walk [n node]
               (if (branch? node)
                 (reduce + n (map (partial walk (inc n)) (children node)))
                 n))]
    (walk 0 root)))

(assert (= 42 (solve example)))

(solve (slurp "day06/input"))
