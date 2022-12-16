(defn fib [n]
  (loop [a 0 b 1 c n]
    (if (zero? c)
      a
      (recur b (+ a b) (dec c)))))

(fib 10)
