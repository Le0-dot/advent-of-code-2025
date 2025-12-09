(ns advent-of-code-2025.day-09 (:gen-class)
    (:require
     [clojure.data.priority-map :refer [priority-map]]
     [clojure.string :refer [split split-lines]]))

(defn parse-coordinates [line]
  (map #(Integer/parseInt %) (split line #",")))

(defn rect-area [[x1 y1] [x2 y2]]
  (let [width  (inc (abs (- x2 x1)))
        height (inc (abs (- y2 y1)))]
    (* width height)))

(defn areas [coordinates]
  (into (priority-map)
        (for [x coordinates
              y coordinates
              :let [area (rect-area x y)]
              :when (not= x y)]
          [area (- area)])))

(defn solution [file]
  (->> file
       slurp
       split-lines
       (map parse-coordinates)
       areas
       first
       first
       println))
