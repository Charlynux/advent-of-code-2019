(require '[intcode :as intcode])

;; remote control program
;; - Accept a movement command via an input instruction.
;; - Send the movement command to the repair droid.
;; - Wait for the repair droid to finish the movement operation.
;; - Report on the status of the repair droid via an output instruction.

;; Movements
(def NORTH 1)
(def SOUTH 2)
(def WEST 3)
(def EAST 4)

(def ->movement
  {NORTH [0 1]
   SOUTH [0 -1]
   EAST [-1 0]
   WEST [-1 0]})

(defn apply-movement [position movement]
  (mapv + position (->movement movement)))

;; Status codes
(def HIT_WALL 0)
(def OK 1)
(def FOUND 2)
(def ERROR 500) ;; NO STATUS RETURN

(defn move [program movement]
  (-> program
      (dissoc :halted)
      (assoc :inputs [movement] :outputs [])
      intcode/run))

(defn chose-move [state] NORTH)

(defn move-state [{:keys [program position] :as state} movement]
  (let [destination (apply-movement position movement)
        updated (move program movement)
        status (get-in updated [:outputs 0] ERROR)]
    (when (= status ERROR)
      (throw (ex-info "No output" updated)))
    (-> state
        (update :board #(assoc % destination status))
        (assoc :program updated
               :position (if (= status HIT_WALL) position destination)))))

;;  { :area { [1, 0] => WALL } :position [0, 0] }
(let [state {:program (intcode/init-program (intcode/read-input-file "day15/input"))
             :position [0 0]
             :board {}}]
  (move state (chose-move state)))
