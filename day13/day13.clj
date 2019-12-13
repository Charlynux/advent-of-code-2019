(require '[intcode :as intcode])

(def input (intcode/read-input-file "day13/input"))

(comment
  (intcode/execute input))

;; partition 3 -> x y tileid
;; 0 -> empty
;; 1 -> wall unbreakable
;; 2 -> block (break by ball)
;; 3 -> horizontal paddle unbreakable
;; 4 -> ball move DIAGONALLY and bounces

(defn solve [input]
  (->> input
       intcode/execute
       :outputs
       (partition 3)
       (filter (fn [[_ _ tile-id]] (= 2 tile-id)))
       count))

(solve input)
