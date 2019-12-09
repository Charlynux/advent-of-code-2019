(ns intcode
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(s/def ::index nat-int?)
(s/def ::inputs (s/coll-of int?))
(s/def ::values (s/coll-of int?))
(s/def ::program (s/keys :req-un [::index ::inputs ::values]))

(def arities {99 0
              1 2
              2 2
              3 0
              4 1
              5 2
              6 2
              7 2
              8 2
              9 1})
(def outputs {99 0
              1 1
              2 1
              3 1
              4 0
              5 0
              6 0
              7 1
              8 1
              9 0})

(defn modes [from number instruction]
  (for [n (range from (+ from number))]
    (mod (quot instruction (int (Math/pow 10 n))) 10)))

(defn read-params [arity instruction {:keys [index values relative-base]}]
  (map-indexed (fn [offset mode]
                 (let [val (get values (+ index offset 1) 0)]
                   (case mode
                     0 (get values val 0)
                     2 (get values (+ relative-base val) 0)
                     val)))
               (modes 2 arity instruction)))
(defn read-outputs [arity n-out instruction {:keys [index values relative-base]}]
  (map-indexed (fn [offset mode]
                 (let [val (+ index arity offset 1)]
                   (case mode
                     0 (get values val 0)
                     2 (+ relative-base (get values val 0))
                     val)))
               (modes (+ 2 arity) n-out instruction)))

(defn parse-instruction [program]
  (let [instruction (get (:values program) (:index program))
        opcode (mod instruction 100)
        arity (get arities opcode 0)
        n-out (get outputs opcode 0)]
    [opcode
     (read-params arity instruction program)
     (read-outputs arity n-out instruction program)]))

(comment
  (parse-instruction {:index 0 :values [1002,4,3,4,33]})
  (parse-instruction {:index 0 :values [1,9,10,3,2,3,11,0,99,30,40,50]})
  (parse-instruction {:index 0 :values [109 10]}))

(defn write-in-values [index value values]
  (assoc (if (< index (count values))
           values
           (vec (concat values (repeat index 0))))
         index value))

(def proceed nil)
(defmulti proceed (fn [opcode & args] opcode))
(defmethod proceed 1 [_ params writes program]
  (-> program
      (update :values (fn [values] (write-in-values (nth writes 0)
                                                   (apply + params)
                                                   values)))
      (update :index (partial + 4))))
(defmethod proceed 2 [_ params writes program]
  (-> program
      (update :values (fn [values] (write-in-values (nth writes 0)
                                                   (apply * params)
                                                   values)))
      (update :index (partial + 4))))
(defmethod proceed 3 [_ _ writes program]
  (if-let [input (-> program :inputs first)]
    (-> program
        (update :values (fn [values] (write-in-values (nth writes 0)
                                                     input
                                                     values)))
        (update :inputs rest)
        (update :index (partial + 2)))
    (assoc program :halted :waiting)))
(defmethod proceed 4 [_ params _ program]
  (-> program
      (update :outputs #(conj % (nth params 0)))
      (update :index (partial + 2))))
(defmethod proceed 5 [_ params _ {:keys [index] :as program}]
  (let [next-index (if (zero? (nth params 0)) (+ index 3) (nth params 1))]
    (-> program
        (assoc :index next-index))))
(defmethod proceed 6 [_ params _ {:keys [index] :as program}]
  (let [next-index (if (zero? (nth params 0)) (nth params 1) (+ index 3))]
    (-> program
        (assoc :index next-index))))
(defmethod proceed 7 [_ params writes program]
  (-> program
      (update :values (fn [values] (write-in-values (nth writes 0)
                                                   (if (apply < params) 1 0)
                                                   values)))
      (update :index (partial + 4))))
(defmethod proceed 8 [_ params writes program]
  (-> program
      (update :values (fn [values] (write-in-values (nth writes 0)
                                                   (if (apply = params) 1 0)
                                                   values)))
      (update :index (partial + 4))))
(defmethod proceed 9 [_ params writes program]
  (-> program
      (update :relative-base (partial + (nth params 0)))
      (update :index (partial + 2))))
(defmethod proceed 99 [_ _ _ program]
  (assoc program :halted :finish))
(defmethod proceed :default [opcode _ _ program]
  (println "Unknown >" opcode)
  (update program :index (partial + 1)))

(defn run-step [program]
  (try
    (let [[opcode params writes] (parse-instruction program)]
      (proceed opcode params writes program))
    (catch Exception e
      (assoc program :halted :failed :error (str (.getSimpleName (class e)) " : " (.getMessage e))))))

(defn run [program]
  (let [result (run-step program)]
    (if (:halted result)
      result
      (recur result))))

(s/fdef run
  :args (s/cat :program ::program)
  :ret ::values
  :fn #(= (-> % :ret count) (-> % :args :values count)))

(comment
  (stest/instrument `run))

(defn init-program [program]
  {
   :index 0
   :relative-base 0
   :inputs []
   :outputs []
   :values program
   })

(defn execute
  ([program] (run (init-program program)))
  ([program input] (run (assoc (init-program program) :inputs [input]))))

(defn read-input-file [file] (read-string (str "[" (slurp file) "]")))

(comment
  (assert (= 1 (-> (execute [3,9,8,9,10,9,4,9,99,-1,8] [8]) :outputs last)))
  (-> (read-input "day05/input") (execute [1]))
  )

