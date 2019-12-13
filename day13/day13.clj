(require '[intcode :as intcode])

(def input (intcode/read-input-file "day13/input"))

(comment
  (intcode/execute input))

;; partition 3 -> x y tileid
;; 0 -> empty
;; 1 -> wall unbreakable
;; 2 -> block (break by ball)
;; 3 -> horizontal paddle unbreakable
;; 4 -> ball move DIAGONALLY and bounces

(defn solve [input]
  (->> input
       intcode/execute
       :outputs
       (partition 3)
       (filter (fn [[_ _ tile-id]] (= 2 tile-id)))
       count))

(solve input)

(def ->tile
  {
   0 " "
   1 "#"
   2 "X"
   3 "="
   4 "O"
   })
(defn print-game [outputs]
  (println "")
  (let [tiles (->> outputs (partition 3) (map (fn [[x y tile]] [[x y] tile])) (into {}))]
    (doseq [y (range 21)]
      (doseq [x (range 40)]
        (print (->tile (get tiles [x y] 0))))
      (println ""))))

(defn predict-future [[pX pY] [cX cY] [targetX targetY]]
  #_(println pX pY cX cY targetX targetY)
  (if (< pY cY)
    (let [deltaY (- targetY cY 1)
          direction (if (< pX cX) + -)
          deltaX (- (direction cX deltaY) targetX)]
      (cond
        (zero? deltaX) 0
        (pos? deltaX) +1
        (neg? deltaX) -1))
    (if (< pX cX) +1 -1)))

(defn round [program]
  (let [tiles (->> program :outputs (partition 3) (map (fn [[x y tile]] [tile [x y]])) (into {}))
        prev-ball-pos (:prev-ball program [0 0])
        ball-pos (get tiles 4)
        pad-pos (get tiles 3)
        movement (predict-future prev-ball-pos ball-pos pad-pos)]
    #_(println "> " movement)
    (-> program
        (dissoc :halted)
        (assoc :inputs [movement])
        (intcode/run)
        (assoc :prev-ball ball-pos))))

(defn index-by-position [outputs]
  (->> outputs (partition 3)
       ;; Index by position to remove duplicates
       (map (fn [[x y tile]] [[x y] tile]))
       (into {})))

(defn blocks? [tiles]
  (->> tiles
       (vals)
       (filter #{2})
       (count)
       pos?))

(println "##############         RESET     #####################")
(map print-game (map :outputs (drop 4990 (take 5000 (iterate round (intcode/run (intcode/init-program (assoc input 0 2))))))))

(defn dot [value] (print ".") value)

(->> (iterate round (intcode/run (intcode/init-program (assoc input 0 2))))
     #_[{:outputs [0 0 2 -1 0 123]} {:outputs [0 0 2 -1 0 123 0 0 0 -1 0 345]}]
     (map (comp dot index-by-position :outputs))
     (drop-while blocks?)
     first
     (#(get % [-1 0])))


(predict-future [0 0] [18 18] [19 19])
