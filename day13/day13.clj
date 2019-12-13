(require '[intcode :as intcode])


(def input (intcode/read-input-file "day13/input"))

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

(println "##############         RESET     #####################")
(map print-game (map :outputs (drop 4990 (take 5000 (iterate round (intcode/run (intcode/init-program (assoc input 0 2))))))))


(predict-future [0 0] [18 18] [19 19])
