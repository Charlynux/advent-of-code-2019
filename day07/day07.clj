(require '[intcode :refer [execute]])

(defn automate [program & inputs]
  (-> (execute program inputs) :outputs last))

(defn apply-sequence [program sequence]
  (->> (automate program (nth sequence 0) 0)
       (automate program (nth sequence 1))
       (automate program (nth sequence 2))
       (automate program (nth sequence 3))
       (automate program (nth sequence 4))))

(comment
  (apply-sequence [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] [4 3 2 1 0])
  (apply-sequence [3,23,3,24,1002,24,10,24,1002,23,-1,23,
                   101,5,23,23,1,24,23,23,4,23,99,0,0] [0,1,2,3,4])
  (apply-sequence [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                   1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] [1,0,4,3,2]))

(def phases (range 0 5))
(defn solve [program]
  (->>
   (for [a phases
         b phases
         c phases
         d phases
         e phases
         :let [s [a b c d e]]
         :when (apply distinct? s)]
     (apply-sequence program s))
   (reduce max)))

(defn read-input [file] (read-string (str "[" (slurp file) "]")))
(solve (read-input "day07/input"))

(solve [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
(solve [3,23,3,24,1002,24,10,24,1002,23,-1,23,
        101,5,23,23,1,24,23,23,4,23,99,0,0])
(solve [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
        1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])
