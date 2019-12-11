(require '[intcode :as intcode])

;; registration identifier
;; emergency hull painting robot <-- Program

;; detect color + paint it to black or white
;; All of the panels are currently black.
;; input instructions
;;   0 if the robot is over a black panel
;;   1 if the robot is over a white panel
;; output
;;   color to paint the panel
;;   direction the robot should turn (0 : left 90°, 1 : right 90° degrees)
;; After turn, forward exactly one panel

;; robot state
;; - position
;; - direction
;; panel state
;; - coord -> color

;; How many panels does it paint at least once?

(def input (intcode/read-input-file "day11/input"))

(defn read-panel [state] (get-in state [:panels (:position state)] 0))

;; Can't find a simpler method...
(defn turn [[directionX directionY] rotation]
  (let [op (if (zero? rotation) - +)
        a (op (Math/atan2 directionY directionX) (/ Math/PI 2))]
    (mapv int [(Math/cos a) (Math/sin a)])))

(defn move-forward [state] (update state :position #(mapv + (:direction state) %)))

(defn update-state [state color rotation]
  (-> state
      (assoc-in [:panels (:position state)] color)
      (update :direction turn rotation)
      move-forward))

;; turn left : [0 1] [1 Ø] [0 -1] [-1 0]

(comment (update-state {:position [0 0] :direction [1 0]} 1 0))

(defn painting-robot [input]
  (let [program (intcode/init-program input)]
    (loop [state { :position [0 0] :direction [1 0] :panels {} }
           program program]
      (let [new-program (intcode/run (dissoc (assoc program :inputs [(read-panel state)] :outputs []) :halted))
            color (get-in new-program [:outputs 0])
            rotation (get-in new-program [:outputs 1])
            new-state (update-state state color rotation)]
        (if (= :waiting (:halted new-program))
          (recur new-state new-program)
          {:state new-state :program new-program})))))


(defn solve [input]
  (-> input
      painting-robot
      (get-in [:state :panels])
      count))

(solve input)
;; 2018
