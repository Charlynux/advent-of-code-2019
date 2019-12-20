(def maze1
  "         A
         A
  #######.#########
  #######.........#
  #######.#######.#
  #######.#######.#
  #######.#######.#
  #####  B    ###.#
  BC...##  C    ###.#
  ##.##       ###.#
  ##...DE  F  ###.#
  #####    G  ###.#
  #########.#####.#
  DE..#######...###.#
  #.#########.###.#
  FG..#########.....#
  ###########.#####
             Z
             Z       ")


(require '[clojure.string :as str])

(defn parse-line [y line]
  (map-indexed (fn [x v] [[x y] v]) line))

(defn parse-input [input]
  (into
   {}
   (comp
    (map-indexed parse-line)
    cat
    (remove (comp #{\space \#} second)))
   (str/split-lines input)))

(defn neighbors [position]
  (map #(mapv + position %) [[1 0] [-1 0] [0 1] [0 -1]]))

(defn point-near [index c]
  (->> (for [[k v] index
             :when (= c v)] k)
       (filter #(some->> % neighbors (map index) (some #{\.})))
       first))

(let [index (parse-input maze1)]
  {:start (point-near index \A)
   :end (point-near index \.)})
