(ns hackerrank)

(defn list-replication [num xs]
  (mapcat (fn [x] (for [i (range num)] x))
       xs))

(defn list-replication-rep [num xs]
  (mapcat #(repeat num %1) xs))

(defn list-replication-partial [num xs]
  (mapcat (partial repeat num ) xs))

(list-replication 4 '(1 2 4))
(list-replication-rep 4 '(1 2 4))
(list-replication-partial 4 '(1 2 4))

(defn fold-left [acc f xs]
  (if (empty? xs)
    acc
    (recur (f acc (first xs)) f (rest xs))))

(defn length [xs]
  (fold-left 0
             (fn [acc next] (+ acc 1))
             xs))

(length [])
(length [4])
(length [2 3 4])

(defn filter-array-fl [delim xs]
  (fold-left []
             (fn [acc next] (if (< next delim) (conj acc next) acc))
             xs))

(defn filter-array-for [delim xs]
  (for [i xs :when (< i delim)] i))

(filter-array-fl 4 [1 8 2 7 3 6 4 5])
(filter-array-for 4 [1 8 2 7 3 6 4 5])
(filter #(< % 4) [1 8 2 7 3 6 4 5])
