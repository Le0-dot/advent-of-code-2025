(ns adevent-of-code-2025.day-01 (:gen-class)
    (:require
     [clojure.math :refer [floor-div]]
     [clojure.string :refer [split-lines]]))

(defrecord Rotation [direction distance])

(defrecord RotationResult [value wraps])

(defn parse-rotation [line]
  (let [direction (subs line 0 1)
        distance (Integer/parseInt (subs line 1))]
    (->Rotation direction distance)))

(defn rotate [current rotation]
  (let [func (case (:direction rotation) "R" + "L" -)
        after (func (:value current) (:distance rotation))
        cropped (mod after 100)
        wraps ((if (neg? after) inc identity) (abs (quot after 100)))]
    (->RotationResult cropped wraps)))

(defn count-zeros [coll]
  (reduce #(if (== (:value %2) 0) (inc %1) %1) 0 coll))

(defn count-wraps [coll] (reduce + 0 (map :wraps coll)))

(defn solution [file]
  (println
   ; (count-zeros
   (count-wraps
    (reductions rotate (->RotationResult 50 0)
                (map parse-rotation
                     (split-lines
                      (slurp file)))))))
