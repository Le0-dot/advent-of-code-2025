(ns advent-of-code-2025.day-06 (:gen-class)
    (:require
     [clojure.string :refer [split split-lines]]))

(defn parse-number-line [line]
  (->> (split line #" ")
       (filter seq)
       (map #(Integer/parseInt %))))

(defn parse-op [s]
  (case s
    "+" +
    "*" *))

(defn parse-op-line [line]
  (->> (split line #" ")
       (filter seq)
       (map parse-op)))

(defn solve-problem [f & args]
  (apply f args))

(defn solution [file]
  (let [lines (->> file
                   slurp
                   split-lines)
        numbers (map parse-number-line (butlast lines))
        ops (parse-op-line (last lines))]
    (println (apply + (apply map solve-problem ops numbers)))))
