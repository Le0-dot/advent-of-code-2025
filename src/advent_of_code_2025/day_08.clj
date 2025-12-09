(ns advent-of-code-2025.day-08 (:gen-class)
    (:require
     [clojure.data.priority-map :refer [priority-map]]
     [clojure.math :refer [pow]]
     [clojure.string :refer [split split-lines]]
     [engelberg.data.union-find :refer [components connect union-find]]))

(defn parse-coordinates [line]
  (map #(Integer/parseInt %) (split line #",")))

(defn distance-square [a b]
  (apply + (map #(pow (- %1 %2) 2) a b)))

(defn distances [coll]
  (into (priority-map)
        (for [x coll, y coll :when (not= x y)]
          [[x y] (distance-square x y)])))

(defn join-closest [uf pm n]
  (if (zero? (dec n))
    uf
    (recur (apply connect uf (first (first pm))) (rest pm) (dec n))))

(defn solution [file]
  (let [coordinates (->> file
                         slurp
                         split-lines
                         (map parse-coordinates))
        pm (distances coordinates)
        uf (apply union-find coordinates)]
    (->> (join-closest uf pm 2000)
         components
         (map count)
         (sort >)
         (take 3)
         (apply *)
         println)))
