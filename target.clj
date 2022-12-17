(defn fib [n]
  (if (< n 2)
    n
    (recur (- n 1) (- n 2))))

(println (fib 10))
