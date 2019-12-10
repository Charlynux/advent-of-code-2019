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

  [(direction [2 2] [1 2]) (direction [2 2] [3 2])]
  )

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

;; PART TWO
;; complete vaporization by giant laser

(defn calc-solution [[x y]] (+ (* 100 x) y))

(defn map-values [f m] (into {} (for [[k v] m] [k (f v)])))

(defn index-asteroids [station asteroids]
  (let [->direction (partial direction station)
        ->distance (partial distance station)]
    (->>  asteroids
          (map (juxt identity (juxt ->direction ->distance)))
          (group-by (comp first second)))))

;; Most important part of this Part 2
(defn angle [[Cx Cy]]
  (let [res (Math/atan2 Cx (- Cy))]
    (if (< res 0)
      (+ (* 2 Math/PI) res)
      res)))

(defn sort-asteroids [indexed-asteroids]
  (let [sort-by-distance #(sort-by (comp second second) < %)
        sort-by-angle #(sort-by (comp angle key) < %)]
    (->> indexed-asteroids
         (map-values sort-by-distance)
         sort-by-angle)))

(defn find-station [asteroids]
  (let [indexation (fn [asteroid] [asteroid
                                  (count (set
                                          (map (partial direction asteroid) asteroids)))])]
    (->>  asteroids
          (map indexation)
          (sort-by second >)
          ffirst)))

(defn solve-part2 [input n]
  (let [asteroids (parse-input input)
        station (find-station asteroids)]
    (->> (index-asteroids station (disj asteroids station))
         sort-asteroids
         vals ;; [[Asteroids of lower angle sorted by distance], [...], [...]]
         (apply interleave)
         (map first) ;; Keep only coordinates
         (drop (dec n))
         first
         calc-solution)))


(comment

  (solve-part2 ".#..##.###...#######
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
###.##.####.##.#..##" 200)

  )
(solve-part2 (slurp "day10/input") 200)
