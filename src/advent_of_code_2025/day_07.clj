(ns advent-of-code-2025.day-07 (:gen-class)
    (:require
     [clojure.set :refer [difference intersection union]]
     [clojure.string :refer [split-lines]]))

(defn diagram-mapping [sym]
  (case sym
    \. nil
    \S :start
    \^ :split
    nil))

(defn parse-line [line]
  (set
   (keep-indexed
    #(when (some? (diagram-mapping %2)) %1)
    line)))

(defn process-line [state line]
  (let [splits (intersection state line)
        new-states (set (mapcat #(vector (dec %) (inc %)) splits))
        non-splits (difference state splits)]
    (union new-states non-splits)))

(defn count-splits [history lines]
  (apply + (map #(count (intersection %1 %2)) history (rest lines))))

(defn solution [file]
  (let [diagram (->> file
                     slurp
                     split-lines
                     (map parse-line))
        start (first (first diagram))
        history (reductions process-line #{start} (rest diagram))]
    (println (count-splits history diagram))))
