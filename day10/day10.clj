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

(defn square [n] (* n n))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn direction [start position]
  (let [rel-pos (mapv - position start)
        g (Math/abs (apply gcd rel-pos))]
    (if (zero? g)
      rel-pos
      (mapv #(/ % g) rel-pos))))

(defn distance [[Ax Ay] [Cx Cy]]
  (Math/sqrt (+ (square (- Ax Cx)) (square (- Ay Cy)))))

(comment
  (direction [0 1] [1 2])
  (distance [0 1] [1 2])

  (direction [0 1] [2 3])
  (distance [0 1] [2 3])

  )

[(direction [2 2] [1 2]) (direction [2 2] [3 2])]

.7..7
.....
67775
....7
...87

(defn count-asteroids [nodes]
  (let [->directions (fn [node] (map (partial direction node) nodes))]
    (map (juxt identity (comp dec count set ->directions)) nodes)))

(defn solve [input]
  (->> input
       parse-input
       count-asteroids
       (map second)
       (reduce max)))


(comment
  (parse-input ".#..#
.....
#####
....#
...##")

  (solve ".#..#
.....
#####
....#
...##")

  (solve "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")

  (solve "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")

  (solve ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")

  (solve ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

  )

(solve (slurp "day10/input"))
