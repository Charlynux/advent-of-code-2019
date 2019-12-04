;; 264793-803935
(time (count (for [a (range 2 9)
                   b (range a 10)
                   c (range b 10)
                   d (range c 10)
                   e (range d 10)
                   f(range e 10)
                   :when (and (or (= a b) (= b c) (= c d) (= d e) (= e f))
                              (<= 264793 (Integer/parseInt (str a b c d e f))  803935))]
               1)))
