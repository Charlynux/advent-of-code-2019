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
   EAST [1 0]
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

;; CURRENT STATE
;; MOVEMENTS => FILTER DESTINATION OPEN => MAP STATE MOVEMENT => PROGRAM-UPDATE (STATUS) => BOARD-UPDATE POSITION-UPDATE

;; Choose best open
;; PUT in closed
;; move prog movement -> updated-prog
;; update board and position
;; STATUS HIT_WALL = RECUR
;; STATUS FOUND OX = STOP
;; STATUS OK...
#_(->> (keys ->movement)
       (map #(apply-movement position %))
       (remove closed?)) ;; Remove closed and better open
;; Merge with open
;; RECUR.

(defn iteration [state]
  (let [node (first (sort-by :priority (vals (:open state))))
        result (move (:program node) (:movement node))
        status (get-in result [:outputs 0] ERROR)
        new-state (-> state
                      (assoc :path-length (:priority node))
                      (update :closed conj (:target node))
                      (update :open dissoc (:target node))
                      (update :board assoc (:target node) status))]
    (condp = status
      FOUND new-state
      HIT_WALL (recur new-state)
      OK (recur (update new-state :open #(merge % (open-neighbors state result node))))
      (throw (ex-info "No output" {:state new-state :program result})))))

(defn closed? [state node]
  (or ((:closed state) (:target node))
      (< (get-in state [:open (:target node) :priority] Integer/MAX_VALUE)
         (:priority node))))

(defn open-neighbors [state program {:keys [target priority]}]
  (let [create-open (fn [movement]
                      {:program program
                       :movement movement
                       :target (apply-movement target movement)
                       :priority (inc priority)})]
    (into {}
          (comp
           (map create-open)
           (remove #(closed? state %))
           (map (juxt :target identity)))
          (keys ->movement))))

(= 4 (count (open-neighbors { :closed #{} :open {} } ["not", "a", "program"] {:target [0 0] :priority 0})))
(= 3 (count (open-neighbors { :closed #{[0 1]} :open {} } ["not", "a", "program"] {:target [0 0] :priority 0})))
(= 3 (count (open-neighbors { :closed #{} :open {[1 0] { :priority 0 }} } ["not", "a", "program"] {:target [0 0] :priority 0})))

(time (let [init-state {:open (open-neighbors { :closed #{} }
                                              (intcode/init-program (intcode/read-input-file "day15/input"))
                                              {:target [0 0] :priority 0})
                        :board {}
                        :priority 0
                        :closed #{[0 0]}}]
        (:path-length (iteration init-state))))

(defn full-exploration [state]
  (if (empty? (:open state))
    state
    (let [node (first (sort-by :priority (vals (:open state))))
          result (move (:program node) (:movement node))
          status (get-in result [:outputs 0] ERROR)
          new-state (-> state
                        (assoc :path-length (:priority node))
                        (update :closed conj (:target node))
                        (update :open dissoc (:target node))
                        (update :board assoc (:target node) status))]
      (condp = status
        HIT_WALL (recur new-state)
        FOUND (recur (update new-state :open #(merge % (open-neighbors state result node))))
        OK (recur (update new-state :open #(merge % (open-neighbors state result node))))
        (throw (ex-info "No output" {:state new-state :program result}))))))

(defn init-a_star [board]
  (let [by-status (group-by second board)
        root (ffirst (get by-status FOUND))]
    {:closed (set (map first (get by-status HIT_WALL)))
     :open { root { :position root :distance 0 }}
     :distances '()}))

(defn fillable-neighbors [state {:keys [ position distance] :as node}]
  (let [create-open (fn [movement]
                      {:position (apply-movement position movement)
                       :distance (inc distance)})
        closed? (fn [node]
                  (or ((:closed state) (:position node))
                      (< (get-in state [:open (:position node) :distance] Integer/MAX_VALUE)
                         (:distance node))))]
    (into {}
          (comp
           (map create-open)
           (remove closed?)
           (map (juxt :position identity)))
          (keys ->movement))))

(fillable-neighbors { :closed #{} } {:position [0 0] :distance 0})
(fillable-neighbors { :closed #{[0 1]} } {:position [0 0] :distance 0})
(fillable-neighbors { :closed #{} :open {[1 0] {:position [1 0] :distance 0}} } {:position [0 0] :distance 0})

(defn fill-oxygen [state]
  (if (empty? (:open state))
    state
    (let [node (first (sort-by :distance (vals (:open state))))
          new-state (-> state
                        (update :distances conj (:distance node))
                        (update :closed conj (:position node))
                        (update :open dissoc (:position node)))]
      (recur (update new-state
                     :open #(merge % (fillable-neighbors new-state node)))))))

(time (let [init-state {:open (open-neighbors { :closed #{} }
                                              (intcode/init-program (intcode/read-input-file "day15/input"))
                                              {:target [0 0] :priority 0})
                        :board {}
                        :priority 0
                        :closed #{[0 0]}}
            board (:board (full-exploration init-state))
            full-state (fill-oxygen (init-a_star board))]
        (reduce max 0 (:distances full-state))))
