(ns fundamentals.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn -main
  [& args]
  (println "Hello, World!"))

(defn evaporator [content, evap_per_day, threshold]
  (def thresh (/ (* content threshold)
                  100))
  (defn helper [content, per_day, thresh, days]
  (if (< content thresh)
    days
    (recur (- content
             (* content per_day))
           per_day
           thresh
           (inc days))))
  (helper content (/ evap_per_day 100) thresh 0))

(evaporator 10 10 10)

(defn disemvowel [string]
  (let [vowels #{"a" "e" "i" "o" "u"}]
    (apply str
           (filter #(not (contains? vowels (clojure.string/lower-case %)))
                   (seq string)))))

(disemvowel "This website is for losers LOL!")

(defn pangram? [s]
  (let [alp (set "abcdefghijklmnopqrstuvwxyz")]
    (= alp
       (set (.replaceAll (.toLowerCase s) "[^a-z]" "")))))

(pangram? "The quick brown fox jumps over the lazy dog.")

(defn to-digits [s]
  (map #(Character/digit % 10) s))

(defmulti to-digits class)
(defmethod to-digits Number [x] (to-digits (str x)))
(defmethod to-digits String [x] (map #(Character/digit % 10) x))

(to-digits "1234")
(to-digits 1234)

(defn desc-order [n]
  (->> (str n)
       (to-digits)
       (sort >)
       (apply str)
       (Integer/parseInt)))

(desc-order 123998)
(desc-order 0)

(defn eureka [from to]
  (->> (range from to)
       (filter #(== % (
digits-sum %)))))

(eureka 90 110)

(defn digital-root [n]
  (def digits (to-digits (str n)))
  (if (= 1 (count digits))
    (first digits)
    (recur (reduce + digits))))

(digital-root 4)
(digital-root 40)
(digital-root 42)

(defn compSame [a b]
  (defn is-join [a b]
    (def left (frequencies (map #(* % %) a)))
    (def right (frequencies b))
    (= left right))
  (if (nil? b)
    false
    (is-join a b)))

(compSame [1 2 1 1 3] [1 1 1 4 9])
(compSame [1 2 11 1 3 11] [1 1 4 121 121 9])

(defn digits-sum [n acc]
  (if (< n 10)
    (+' acc n)
    (digits-sum (quot n 10) (+' acc (mod n 10)))))












