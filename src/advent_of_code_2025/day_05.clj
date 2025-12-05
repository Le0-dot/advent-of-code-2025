(ns advent-of-code-2025.day-05 (:gen-class)
    (:require
     [clojure.string :refer [split split-lines]]))

(defrecord InclusiveRange [start end]
  java.lang.Comparable
  (compareTo [self other] (compare start (:start other))))

(defn in-range? [^InclusiveRange {:keys [start end]} n]
  (<= start n end))

(defn in-any-range? [coll n]
  (some #(in-range? % n) coll))

(defn overlap? [^InclusiveRange left ^InclusiveRange right]
  (or (in-range? left (:start right))
      (in-range? left (:end right))
      (in-range? right (:start left))
      (in-range? right (:end left))))

(defn join-ranges [^InclusiveRange left ^InclusiveRange right]
  (->InclusiveRange
   (min (:start left) (:start right))
   (max (:end left) (:end right))))

(defn ranges-cons [coll new-range]
  (let [{overlapping true other false} (group-by #(overlap? % new-range) coll)]
    (cons
     (reduce join-ranges new-range overlapping)
     other)))

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
    ; (->> numbers
    ;      (filter #(in-any-range? fresh %))
    ;      count
    ;      println)))
    (->> fresh
         sort
         (reduce ranges-cons '())
         (map #(- (:end %) (:start %) -1))
         (apply +)
         println)))
