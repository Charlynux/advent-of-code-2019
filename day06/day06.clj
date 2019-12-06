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

(defn solve2 [input]
  (let [datas (parse-input input)
        links (reduce
               (fn [acc [k v]]
                 (-> acc
                     (update k (fnil #(conj % v) #{}))
                     (update v (fnil #(conj % k) #{}))))
               {}
               datas)
        viewed (atom #{})
        branch? (constantly true)
        children #(some-> % links (clojure.set/difference @viewed) seq)
        ;; Like tree-seq but with sum depth
        walk (fn walk [n node]
               (swap! viewed conj node)
               (if (not= node "SAN")
                 (if (branch? node)
                   (reduce + (map (partial walk (inc n)) (children node)))
                   0)
                 n))]
    (- (walk 0 "YOU") 2)))

(def example2 "COM)B
  B)C
  C)D
  D)E
  E)F
  B)G
  G)H
  D)I
  E)J
  J)K
  K)L
  K)YOU
  I)SAN")

(assert (= 4 (solve2 example2)))

(solve2 (slurp "day06/input"))
