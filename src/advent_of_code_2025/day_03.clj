(ns advent-of-code-2025.day-03 (:gen-class)
    (:require
     [clojure.string :refer [split-lines]]))

(defn parse-bank [line] (map #(Character/digit % 10) line))

(defn split-on-max [coll]
  (let [max-value (apply max coll)
        pred #(not= % max-value)]
    [(take-while pred coll)
     max-value
     (rest (drop-while pred coll))]))

(defn max-n [n coll]
  (cond
    (zero? n) []
    (empty? coll) []
    :else (let [[left value right] (split-on-max coll)
                on-the-right (max-n (dec n) right)
                needed (- n 1 (count on-the-right))]
            (apply conj (max-n needed left) value on-the-right))))

(defn digits-to-number [coll] (reduce #(+ (* 10 %1) %2) coll))

(defn solution [file]
  (->>
   (-> file slurp split-lines)
   (map parse-bank) (map #(max-n 12 %)) (map digits-to-number) (apply +) println))
