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
(defn print-game [tiles]
  (println "")
  (doseq [y (range 21)]
    (doseq [x (range 40)]
      (print (->tile (get tiles [x y] 0))))
    (println "")))

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
  (let [board (into (:board program {})
                    (comp (partition-all 3)
                          (map (fn [[x y tile]] [[x y] tile])))
                    (:outputs program))
        tiles (into {} (for [[coords tile] board] [tile coords]))
        prev-ball-pos (:prev-ball program [0 0])
        ball-pos (get tiles 4)
        pad-pos (get tiles 3)
        movement (predict-future prev-ball-pos ball-pos pad-pos)]
    #_(println "> " movement)
    (-> program
        (dissoc :halted)
        (assoc :inputs [movement] :outputs [])
        (intcode/run)
        (assoc :prev-ball ball-pos :board board))))

(defn blocks? [tiles]
  (->> tiles
       (vals)
       (filter #{2})
       (count)
       pos?))

(defn dot [value] (print ".") value)

(time (->> (iterate round (intcode/run (intcode/init-program (assoc input 0 2))))
           (drop 1)
           (map (comp dot :board))
           (drop-while blocks?)
           first
           (#(get % [-1 0]))))
