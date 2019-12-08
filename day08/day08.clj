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

;; 0 is black, 1 is white, and 2 is transparent
;; first layer in front and the last layer in back
;; 2 2 0 1 -> 0
(def ->char { 0 " " 1 "X"})
(defn solve2 [width height input]
  (let [digits (parse-input input)
        layer-size (* width height)
        n-layers (quot (count digits) layer-size)]
    (->> digits
         (partition layer-size)
         (apply interleave)
         (partition n-layers)
         (map #(first (drop-while #{2} %)))
         (partition width))))

(clojure.pprint/pprint (solve2 2 2 "0222112222120000"))

(clojure.pprint/pprint (solve2 25 6 (slurp "day08/input")))
