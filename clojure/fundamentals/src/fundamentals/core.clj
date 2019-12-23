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

(defn digits-sum [n acc]
  (if (< n 10)
    (+' acc n)
    (digits-sum (quot n 10) (+' acc (mod n 10)))))

(defn eureka [from to]
  (->> (range from to)
       (filter #(== % (digits-sum %)))))

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

(defn is-merge-check [str p i]
  (if (empty? p)
    true
    (let [fp (first p)
          ni (string/index-of str fp)
          res (> ni i)]
      (and res (is-merge-check str (rest p) ni)))))

(defn is-merge [str p1 p2]
  (and
    (= (set (char-array str)) (set (char-array (concat p1 p2))))
    (is-merge-check str p1 -1)
    (is-merge-check str p2 -1)))

(is-merge "codewars" "cdw" "oears")
(is-merge "codewars" "cod" "war")
(is-merge "codewars" "codes" "wars")
(is-merge-check "codewars" "cdw" -1)
(is-merge-check "codewars" "oears" -1)

(defn sockMerchant [n ar]
  (->> ar
       (frequencies)
       (vals)
       (map #(quot % 2))
       (reduce +)))

(sockMerchant 9 [10 20 20 10 10 30 50 10 20])

(defn countingValleys [n s]
  (letfn [(op [acc next]
            (let [[valls count] acc]
              (cond
                (and (= next \U) (= -1 count)) [(inc valls) (inc count)]
                (= next \D) [valls (dec count)]
                :else [valls (inc count)])))]
    (first (reduce op [0 0] s))))

(countingValleys 42 "UDDDUDUU")
(countingValleys 42 "DDUUUUDD")

(defn jumpingOnClouds [c]
  (letfn [(help [xs count]
            (let [[f s t & tail] xs]
              (cond
                (nil? s) count
                (= 0 t) (recur (conj tail t) (inc count))
                :else (recur (conj tail t s) (inc count)))))]
    (help c 0)))

(jumpingOnClouds [0 1 0 0 0 1 0])

(quot 5 3)
(rem 5 3)

(get (frequencies "asdff") \a)

(defn repeatedString [s n]
  (letfn [(count-a [s] (or (get (frequencies s) \a) 0))]
    (let [size (count s)
          whole (quot n size)
          remains (rem n size)
          as-in-whole (count-a s)
          as-in-wholes (* whole as-in-whole)]
      (if (= remains 0)
        as-in-wholes
        (+ as-in-wholes (count-a (take remains s)))))))

(repeatedString "aba" 10)
(repeatedString "a" 1000000000000)
(repeatedString "cbd" 817723)

(defn hourglassSum [arr]
  (letfn [(ag [i j] (get-in arr [i j] 0))
          (hourglass [i j]
            (+ (ag i j)
               (ag i (+ j 1))
               (ag i (+ j 2))
               (ag (+ i 1) (+ j 1))
               (ag (+ i 2) j)
               (ag (+ i 2) (+ j 1))
               (ag (+ i 2) (+ j 2))))
          (sum [] (for [i (range 0 (- (count arr) 2))
                        j (range 0 (- (count (nth arr i)) 2))]
                    (hourglass i j)))]
    (apply max (sum))))

(def v [[-9 -9 -9 1 1 1]
        [0 -9 0 4 3 2]
        [-9 -9 -9 1 2 3]
        [0 0 8 6 6 0]
        [0 0 0 -2 0 0]
        [0 0 1 2 4 0]])

(hourglassSum v)

(defn rotLeft [a d]
  (let [[head tail] (split-at d a)]
    (concat tail head)))

(rotLeft [1 2 3 4 5] 2)

(defn minimumBribes [q]
  (let [x (reduce (fn [[shouldbe pred sum] real]
                    (let [diff (- real shouldbe)
                          next-i (inc shouldbe)]
                      (println (str "real " real " shouldbe " shouldbe " diff " diff))
                      (cond (> diff 2) (reduced "Too chaotic")
                            (> diff 0) [next-i real (+ diff sum)]
                            (> pred real) [next-i real (inc sum)]
                            :else [next-i real sum]))) [1 0 0] q)
        do (println "x " x)
        res (if (string? x)
              x
              (nth x 2))]
    (println res)))

(minimumBribes [1 2 3 4 5])
(minimumBribes [2 1 5 3 4])
(minimumBribes [2 5 1 3 4])
(minimumBribes [1 2 5 3 7 8 6 4])
(minimumBribes [1 2 5 3 7 8 4 6])
(minimumBribes [1 2 3 4 5 6 7 8])


(defn checkMagazine [magazine note]
  (let [note-freq (frequencies note)
        mag-freq (frequencies magazine)]
    (if (every? (fn [[k v]]
                  (>= (get mag-freq k 0) v)) note-freq)
      (println "Yes")
      (println "No"))))

(defn split [s]
  (str/split s #" "))

(checkMagazine (split "ive got a lovely bunch of coconuts")
               (split "ive got some coconuts"))

(checkMagazine (split "apgo clm w lxkvg mwz elo bg elo lxkvg elo apgo apgo w elo bg")
               (split "elo lxkvg bg mwz clm w"))

(checkMagazine ["give" "me" "one" "grand" "today" "night"]
               ["give" "one" "grand" "today"])

(defn twoStrings [s1 s2]
  (let [set1 (set s1)
        set2 (set s2)]
    (if (some set1 set2)
      "YES"
      "NO")))

(twoStrings "alal" "a")
(twoStrings "alal" "b")
(twoStrings "alal shma" "b")
(twoStrings "alal shma" "sh")

(defn maximumToys [prices k]
  (letfn [(help [[head & tail] rem count]
            (if (or (= 0 rem) (> head rem))
              count
              (help tail (- rem head) (inc count))))]
    (help (sort prices) k 0)))

(maximumToys [2 3 4 1] 7)
(maximumToys [1 12 5 111 200 1000 10] 50)

(use 'clojure.data)
(use 'clojure.set)

(defn makeAnagram [a b]
  (let [fa (frequencies a)
        fb (frequencies b)
        all (set (concat a b))]
    (reduce (fn [acc x] (let [vb (get fb x 0)
                              va (get fa x 0)
                              diff (Math/abs ^int (- va vb))]
                          (+ acc diff)))
            0 all)))

(makeAnagram "ala" "mak")

(defn alternatingCharacters [[h & t]]
  (letfn [(op [[acc pred] next]
            (if (= pred next) [(inc acc) next]
                              [acc next]))]
    (first (reduce op [0 h] t))))

(alternatingCharacters "ABABABABAB")
(alternatingCharacters "AAAA")
(alternatingCharacters "AAABBB")

(defn minimumAbsoluteDifference [arr]
  (let [sorted (sort arr)
        diff #(Math/abs ^int (- %1 %2))]
    (apply min (map diff sorted (rest sorted)))))

(minimumAbsoluteDifference [3 -7 0])
(minimumAbsoluteDifference [-59 -36 -13 1 -53 -92 -2 -96 -54 75])

(defn luckBalance [k contests]
  (let [[sum ls] (reduce (fn [[count ls] [l i]] (if (= 0 i) [(+ count l) ls]
                                                            [count (conj ls l)]))
                         [0 []] contests)
        priority (sort > ls)
        to-add (reduce + (take k priority))
        to-remove (reduce + (drop k priority))]
    (- (+ sum to-add)
       to-remove)))

(luckBalance 3 [[5 1] [2 1] [1 1] [8 1] [10 0] [5 0]])

(defn whatFlavors [cost money]
  (reduce (fn [[m i] next]
            (prn "m next i" m next i)
            (let [compensant-i (get m (- money next))]
              (if (nil? compensant-i) [(assoc m next i) (inc i)]
                                      (reduced (if (< i compensant-i)
                                                 (prn i compensant-i)
                                                 (prn compensant-i i))))))
          [{} 1] cost))

(whatFlavors [2,1,3,5,6] 5)














