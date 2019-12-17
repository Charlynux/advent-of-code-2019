(require '[intcode :as intcode])

(def result (intcode/execute (intcode/read-input-file "day17/input")))

(def camera-view (->> result
                      :outputs
                      (map char)
                      (partition-by #{\newline})
                      (remove #(= 1 (count %)))))

(defn index-line [y line]
  (map-indexed (fn [x v]
                 [[x y] v]) line))

(def indexed-view
  (into {} (comp (map-indexed index-line) cat) camera-view))
