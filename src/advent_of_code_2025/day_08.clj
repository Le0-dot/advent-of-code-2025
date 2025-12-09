(ns advent-of-code-2025.day-08 (:gen-class)
    (:require
     [clojure.data.priority-map :refer [priority-map]]
     [clojure.math :refer [pow]]
     [clojure.string :refer [split split-lines]]
     [engelberg.data.union-find :refer [connect connected? count-components
                                        union-find]]))

(defn parse-coordinates [line]
  (map #(Integer/parseInt %) (split line #",")))

(defn distance-square [a b]
  (apply + (map #(pow (- %1 %2) 2) a b)))

(defn distances [coll]
  (into (priority-map)
        (for [x coll, y coll :when (not= x y)]
          [[x y] (distance-square x y)])))

(defn join-closest [uf pm]
  (let [[x y] (first (first pm))]
    (if (and (= 2 (count-components uf)) (not (connected? uf x y)))
      [x y]
      (recur (connect uf x y) (rest pm)))))

(defn solution [file]
  (let [coordinates (->> file
                         slurp
                         split-lines
                         (map parse-coordinates))
        pm (distances coordinates)
        uf (apply union-find coordinates)]
    (->> (join-closest uf pm)
         (map first)
         (apply *)
         println)))
