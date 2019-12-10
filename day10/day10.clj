(require '[clojure.string :as str])

(defn parse-input [input]
  (->>
   (str/split input #"\n")
   (map #(str/split % #""))
   (map-indexed (fn [y line]
                  (map-indexed (fn [x v] (when (= "#" v) [x y])) line)))
   (mapcat identity)
   (filter some?)
   (into #{})))

(comment
  (parse-input ".#..#
.....
#####
....#
...##"))
