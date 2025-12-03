(ns advent-of-code-2025.day-03 (:gen-class)
    (:require
     [clojure.string :refer [split-lines]]))

(defn parse-bank [line] (map #(Character/digit % 10) line))

(defn max-pair [coll]
  (let [mid-max (apply max coll)
        right-partition (next (drop-while #(not= % mid-max) coll))
        left-partition (take-while #(not= % mid-max) coll)]
    (if (seq right-partition)
      [mid-max (apply max right-partition)]
      [(apply max left-partition) mid-max])))

(defn pair-to-number [[a b]] (+ (* 10 a) b))

(defn solution [file]
  (->>
   (-> file slurp split-lines)
   (map parse-bank) (map max-pair) (map pair-to-number) (apply +) println))
