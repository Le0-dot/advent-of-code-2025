(ns advent-of-code-2025.day-11 (:gen-class)
    (:require
     [clojure.string :refer [split-lines]]))

(defn parse-line [line]
  (let [[[k] v] (->> line
                     (re-seq #"\w+")
                     (split-at 1))]
    {k v}))

(defn find-paths
  ([nodes] (find-paths nodes (list "you") 0))
  ([nodes stack n]
   (let [top (peek stack)]
     (cond
       (empty? stack) n
       (= top "out") (recur nodes (pop stack) (inc n))
       :else (recur nodes (apply conj (pop stack) (nodes top)) n)))))

(defn solution [file]
  (->> file
       slurp
       split-lines
       (map parse-line)
       (apply merge)
       find-paths
       println))
