;; module -> mass / 3 -> round down -> - 2
(require '[clojure.string :as str])

(defn fuel-required [mass]
  (-> mass
      (quot 3)
      (- 2)))

(assert (= (fuel-required 12) 2))
(assert (= (fuel-required 14) 2))
(assert (= (fuel-required 1969) 654))
(assert (= (fuel-required 100756) 33583))

(->> (str/split (slurp "input") #"\n")
     (map read-string)
     (map fuel-required)
     (reduce + 0))
;; 3495189
