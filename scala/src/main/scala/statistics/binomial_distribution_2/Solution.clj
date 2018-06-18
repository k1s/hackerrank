(defn factorial [n]
  (reduce *' (range 1 (inc n))))

(defn n-by-x [n x]
  (/ (factorial n)
     (* (factorial x)
        (factorial (- n x)))))

(defn binomial [y]
  (defn binomial-help [x n p q]
    (* (n-by-x n x)
       (Math/pow p x)
       (Math/pow q (- n x))))
  (let [p 0.12]
    (binomial-help y 6 p (- 1 p))))


(printf "%.3f" (reduce + (map binomial (range 3 7))))
 
