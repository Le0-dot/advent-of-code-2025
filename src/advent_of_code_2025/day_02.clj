(ns advent-of-code-2025.day-02 (:gen-class)
    (:require
     [clojure.string :refer [split]]))

(def inclusive-range #(range %1 (inc %2)))

(defn parse-id-range [repr]
  (->> (split repr #"-")
       (map #(Long/parseLong %))
       (apply inclusive-range)))

(defn partition-into
  "Returns a lazy sequence of n lists of elements from coll."
  [n coll]
  (partition-all (-> coll count (/ n)) coll))

(defn same? [coll] (apply = coll))

(defn divisible? [a b] (zero? (mod a b)))

; (defn invalid-id? [id]
;   (->> id str (partition-into 2) same?))

(defn invalid-id? [id]
  (let [repr (str id)
        len (count repr)]
    (some true?
          (for [x (inclusive-range 2 len) :when (divisible? len x)]
            (->> repr (partition-into x) same?)))))

(defn solution [file]
  (->>
   (-> file slurp (split #","))
   (mapcat parse-id-range) (filter invalid-id?) (apply +) println))
