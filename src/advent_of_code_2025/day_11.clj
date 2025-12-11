(ns advent-of-code-2025.day-11 (:gen-class)
    (:require
     [clojure.string :refer [split-lines]]))

(defn parse-line [line]
  (let [[[k] v] (->> line
                     (re-seq #"\w+")
                     (split-at 1))]
    {k v}))

(defn count-paths
  ([nodes start end] ((count-paths nodes start end {}) start))
  ([nodes start end visited]
   (cond
     (= start end) (assoc visited end 1)
     (visited start) visited
     :else (let [children (nodes start)
                 visited-children (reduce #(count-paths nodes %2 end %1) visited children)
                 found-paths (->> children (select-keys visited-children) vals (apply +))]
             (assoc visited-children start found-paths)))))

(defn solution [file]
  (let [nodes (->> file
                   slurp
                   split-lines
                   (map parse-line)
                   (apply merge))]
    (->> [["svr" "fft"]
          ["fft" "dac"]
          ["dac" "out"]]
         (map #(apply count-paths nodes %))
         (apply *)
         println)))
