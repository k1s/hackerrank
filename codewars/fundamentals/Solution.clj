(ns codewars)


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

(defn desc-order [n]
  (->> (str n)
       (map #(Character/digit % 10))
       (sort >)
       (apply str)
       (Integer/parseInt)))


(desc-order 123998)
(desc-order 0)

