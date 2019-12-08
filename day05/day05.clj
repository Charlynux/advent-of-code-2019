(require '[intcode :refer [execute read-input-file]])

(-> (read-input-file "day05/input")
    (execute 1)
    :outputs
    last)
;; 7286649

(-> (read-input-file "day05/input")
    (execute 5)
    :outputs
    last)
;; 15724522
