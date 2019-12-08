;; puzzle input : Picture of the BIOS password
;; format : Space Image Format
;;;;; series of digits that each represent the color of a single pixel
;;;;; digits fill each row of the image left-to-right
;;;;; Note: partition ?

;; image = series of identically-sized layers
(comment
  (partition 2 (partition 3 "123456789012")))

;; width 25 height 6
;; fewest 0 digits
;; the number of 1 digits multiplied by the number of 2 digits

(defn parse-input [input]
  (map #(Integer/parseInt %)
       (clojure.string/split input #"")))

(->> (slurp "day08/input")
     parse-input
     (partition (* 25 6))
     (map frequencies)
     (sort-by #(get % 0 0))
     first
     ((fn [freqs] (* (get freqs 1) (get freqs 2)))))
