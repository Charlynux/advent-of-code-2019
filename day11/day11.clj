(require '[intcode :as intcode])

;; registration identifier
;; emergency hull painting robot <-- Program

;; detect color + paint it to black or white
;; All of the panels are currently black.
;; input instructions
;;   0 if the robot is over a black panel
;;   1 if the robot is over a white panel
;; output
;;   color to paint the panel
;;   direction the robot should turn (0 : left 90°, 1 : right 90° degrees)
;; After turn, forward exactly one panel

;; robot state
;; - position
;; - direction
;; panel state
;; - coord -> color

;; How many panels does it paint at least once?
