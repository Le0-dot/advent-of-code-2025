(ns advent-of-code-2025.day-04 (:gen-class)
    (:require
     [clojure.string :refer [split-lines]]))

(defn parse-line [line]
  (map #(case % \. :empty \@ :roll) line))

(defn pad-with [padding coll] (concat [padding] coll [padding]))

(defn pad-with-empty [coll]
  (->> coll
       (map #(pad-with :empty %))
       (pad-with (-> coll first count inc inc (repeat :empty)))))

(defn vertical-partitions [n coll]
  (->> coll
       (map #(partition n 1 %))
       (apply map concat)))

(defn square-partitions [n coll]
  (->> coll
       (partition n 1)
       (mapcat #(vertical-partitions n %))))

(defn not-empty-center? [coll]
  (let [n (quot (count coll) 2)
        mid (nth coll n)]
    (not= mid :empty)))

(defn accesssible? [coll]
  (->> coll
       (filter #(not= % :empty))
       count
       (>= 4)))

(defn count-accessible [coll]
  (->> coll
       (filter accesssible?)
       count))

(defn solution [file]
  (->> file
       slurp
       split-lines
       (map parse-line)
       pad-with-empty
       (square-partitions 3)
       (filter not-empty-center?)
       count-accessible
       println))
