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
              8 2})
(def outputs {99 0
              1 1
              2 1
              3 1
              4 0
              5 0
              6 0
              7 1
              8 1})

(defn modes [from number instruction]
  (for [n (range from (+ from number))]
    (zero? (mod (quot instruction (int (Math/pow 10 n))) 10))))

(defn read-params [arity instruction {:keys [index values]}]
  (map-indexed (fn [offset pos-mode?]
                 (let [val (get values (+ index offset 1))]
                   (if pos-mode?
                     (get values val)
                     val)))
               (modes 2 arity instruction)))
(defn read-outputs [arity n-out instruction {:keys [index values]}]
  (map-indexed (fn [offset pos-mode?]
                 (let [val (+ index arity offset 1)]
                   (if pos-mode?
                     (get values val)
                     val)))
               (modes (+ 2 arity) n-out instruction)))

(defn parse-instruction [program]
  (let [instruction (get (:values program) (:index program))
        opcode (mod instruction 100)
        arity (arities opcode)
        n-out (outputs opcode)]
    [opcode
     (read-params arity instruction program)
     (read-outputs arity n-out instruction program)]))

(comment
  (parse-instruction {:index 0 :values [1002,4,3,4,33]})
  (parse-instruction {:index 0 :values [1,9,10,3,2,3,11,0,99,30,40,50]}))

(def proceed nil)
(defmulti proceed (fn [opcode & args] opcode))
(defmethod proceed 1 [_ params writes program]
  (-> program
      (update :values (fn [values] (assoc values (nth writes 0)
                                         (apply + params))))
      (update :index (partial + 4))))
(defmethod proceed 2 [_ params writes program]
  (-> program
      (update :values (fn [values] (assoc values (nth writes 0)
                                         (apply * params))))
      (update :index (partial + 4))))
(defmethod proceed 3 [_ _ writes program]
  (if-let [input (-> program :inputs first)]
    (-> program
        (update :values (fn [values] (assoc values (nth writes 0)
                                           input)))
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
      (update :values (fn [values] (assoc values (nth writes 0)
                                         (if (apply < params) 1 0))))
      (update :index (partial + 4))))
(defmethod proceed 8 [_ params writes program]
  (-> program
      (update :values (fn [values] (assoc values (nth writes 0)
                                         (if (apply = params) 1 0))))
      (update :index (partial + 4))))
(defmethod proceed 99 [_ _ _ program]
  (assoc program :halted :finish))

(defn run [program]
  (let [[opcode params writes] (parse-instruction program)
        result (proceed opcode params writes program)]
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
   :inputs []
   :outputs []
   :values program
   })

(defn execute
  ([program] (execute program []))
  ([program input] (run {
                         :index 0
                         :inputs [input]
                         :outputs []
                         :values program
                         })))

(defn read-input-file [file] (read-string (str "[" (slurp file) "]")))

(comment
  (assert (= 1 (-> (execute [3,9,8,9,10,9,4,9,99,-1,8] [8]) :outputs last)))
  (-> (read-input "day05/input") (execute [1]))
  )

