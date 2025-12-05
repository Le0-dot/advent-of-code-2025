(ns advent-of-code-2025.day-05 (:gen-class)
    (:require
     [clojure.string :refer [split split-lines]]))

(defrecord InclusiveRange [start end])

(defn in-range? [^InclusiveRange {:keys [start end]} n]
  (<= start n end))

(defn in-any-range? [coll n]
  (some #(in-range? % n) coll))

(defn parse-range-line [line]
  (apply ->InclusiveRange (map #(Long/parseLong %) (split line #"-"))))

(defn parse-lines [lines]
  (let [n (.indexOf lines "")
        [range-lines rest-lines] (split-at n lines)
        number-lines (rest rest-lines)]
    [(map parse-range-line range-lines)
     (map #(Long/parseLong %) number-lines)]))

(defn solution [file]
  (let [[fresh numbers] (->> file
                             slurp
                             split-lines
                             parse-lines)]
    (->> numbers
         (filter #(in-any-range? fresh %))
         count
         println)))
