(ns advent-of-code-2025.day-01 (:gen-class)
    (:require
     [clojure.math :refer [floor-div]]
     [clojure.string :refer [split-lines]]))

(defrecord Rotation [direction distance])

(defrecord RotationResult [value wraps])

(defn parse-rotation [line]
  (let [direction (case (get line 0) \R :right \L :left)
        distance (Integer/parseInt (subs line 1))]
    (->Rotation direction distance)))

(defn rotation-to-angle [rotation]
  ((case (:direction rotation) :right + :left -) (:distance rotation)))

(defn on [f g] #(apply f (map g %&)))

(defn of [f g] #(g (apply f %&)))

(defn rotate [current rotation]
  (let [before (:value current)
        after (+ before (rotation-to-angle rotation))
        modif (case (:direction rotation) :right identity :left dec)
        wrapped-times #(-> % modif (floor-div 100))
        wraps ((on (of - abs) wrapped-times) before after)]
    (->RotationResult after wraps)))

(defn count-zeros [coll]
  (count (filter #(-> % (mod 100) zero?) (map :value coll))))

(defn count-wraps [coll] (apply + (map :wraps coll)))

(defn solution [file]
  (let [scan (reductions rotate (->RotationResult 50 0)
                         (map parse-rotation
                              (split-lines
                               (slurp file))))]
    (println
     (count-zeros scan)
     (count-wraps scan))))
