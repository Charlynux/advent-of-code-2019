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

(let [a (Math/toRadians 90)] [(Math/cos a) (Math/sin a)])

(Math/asin 0)
(Math/acos 0)

(defn square [n] (* n n))

(defn calc [[Ax Ay] [Cx Cy]]
  (Math/toDegrees
   (let [[Bx By] [(inc Ax) Ay]
         a (Math/sqrt  (+ (square (- Bx Cx)) (square (- By Cy))))
         b (Math/sqrt  (+ (square (- Ax Cx)) (square (- Ay Cy))))
         c (Math/sqrt  (+ (square (- Bx Ax)) (square (- By Ay))))]
     (Math/acos (/ (- (+ (square b) (square c)) (square a)) (* 2 b c))))))

(calc [0 0] [0 1])
(calc [0 0] [0 -1])
