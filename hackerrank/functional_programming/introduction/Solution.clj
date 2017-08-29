(ns hackerrank
  (:require [clojure.string :as str]))

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
  (reduce (fn [acc next] (+ acc 1)) 0 xs))

(length [])
(length [4])
(length [2 3 4])

(defn filter-array-for [delim xs]
  (for [i xs :when (< i delim)] i))

(defn filter-array [f xs]
  (fold-left []
             (fn [acc next] (if (f next) (conj acc next) acc))
             xs))

(filter-array-for 4 [1 8 2 7 3 6 4 5])
(filter-array #(< % 4) [1 8 2 7 3 6 4 5])

(defn filter-odd [xs]
  (map #(first %)
       (filter #(odd? (second %))
               (map vector xs (range 0 (count xs))))))

(filter-odd [1 2 3 4 5 6 7 8 9])

(defn reverse-r [xs]
  (reduce conj [] xs))

(reverse-r [1 2 3 4])

(defn sum-odd-1 [xs]
  (reduce (fn [acc next] (if (odd? next) (+ acc next) acc))
          0
          xs))

(defn sum-odd-2 [xs]
  (reduce + (filter odd? xs)))

(sum-odd-1 [1 2 3 4 5])
(sum-odd-2 [1 2 3 4 5])


(defn absolute [xs]
  (map #(reduce * (range 1 (inc x))))(Math/abs %) xs))

(absolute [1 -1 2 -2 3 -3])

(defn exp [x]
  (+ 1
     (reduce + (map #(/
                       (Math/pow x %)
                       (reduce * (range 1 (inc %))))
                     (range 1 10)))))

(exp 20.0)

(defn volume [x]
  (def coeffs (map
               read-string 
               (str/split
                 (read-line)
                 " "))))










