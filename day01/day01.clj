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

;; negative -> 0

(defn fuel-required-2 [mass]
  (->> (iterate fuel-required mass)
       (drop 1) ;; keep only fuel masses
       (take-while pos?)
       (reduce + 0)))

(assert (= (fuel-required-2 14) 2))
(assert (= (fuel-required-2 1969) 966))
(assert (= (fuel-required-2 100756) 50346))

(->> (str/split (slurp "input") #"\n")
     (map read-string)
     (map fuel-required-2)
     (reduce + 0))
;; 5239910
