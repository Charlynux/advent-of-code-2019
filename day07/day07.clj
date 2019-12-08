(require '[intcode :refer [execute run init-program]])

(defn amplifier [program phase]
  (let [program (atom (run (assoc program :inputs [phase])))]
    (fn [input]
      (let [result (run (dissoc (assoc @program :inputs [input]) :halted))]
        (reset! program result)
        (println result)
        result))))

(defn read-output [program] (-> program :outputs last))

(defn init-amplifiers [program sequence]
  (map (partial amplifier program) sequence))

(defn run-amplifiers [amplifiers input]
  (reduce
   (fn [result amplifier] (amplifier (read-output result)))
   ((first amplifiers) input)
   (rest amplifiers)))

(defn apply-sequence [program sequence init]
  (let [amplifiers (init-amplifiers program sequence)]
    (run-amplifiers amplifiers init)))

(comment
  (apply-sequence (init-program [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]) [4 3 2 1 0] 0)
  (apply-sequence (init-program [3,23,3,24,1002,24,10,24,1002,23,-1,23,
                                 101,5,23,23,1,24,23,23,4,23,99,0,0]) [0,1,2,3,4] 0)
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
     (read-output (apply-sequence program s 0)))
   (reduce max)))

(comment
  (solve [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
  (solve [3,23,3,24,1002,24,10,24,1002,23,-1,23,
          101,5,23,23,1,24,23,23,4,23,99,0,0])
  (solve [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
          1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]))

(defn read-input [file] (read-string (str "[" (slurp file) "]")))
(solve (read-input "day07/input"))

(defn run-until-halted [program sequence]
  (let [amplifiers (init-amplifiers (init-program program) sequence)]
    (loop [input 0]
      (let [result (run-amplifiers amplifiers input)]
        (println "OUT>" (read-output result))
        (println "Halted" (:halted result))
        (if (= :finish (some-> result :halted))
          (read-output result)
          (recur (read-output result)))))))

(comment
  (run-until-halted [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                     27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
                    [9,8,7,6,5])
  (run-until-halted [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                     -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                     53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
                    [9,7,8,5,6]))

(def phases-2 (range 5 10))
(defn solve-2 [program]
  (->>
   (for [a phases-2
         b phases-2
         c phases-2
         d phases-2
         e phases-2
         :let [s [a b c d e]]
         :when (apply distinct? s)]
     (run-until-halted program s))
   (reduce max)))

(comment
  (solve-2 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
            27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]))

(time (solve-2 (read-input "day07/input")))
