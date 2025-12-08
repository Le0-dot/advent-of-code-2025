(ns advent-of-code-2025.day-07 (:gen-class)
    (:require
     [clojure.set :refer [difference intersection union]]
     [clojure.string :refer [split-lines]]))

(defrecord State [positions split-count])

(defn init-state [start]
  (->State {start 1} 0))

(defn diagram-mapping [sym]
  (case sym
    \. nil
    \S :start
    \^ :split
    nil))

(defn parse-line [line]
  (keep-indexed
   #(when (some? (diagram-mapping %2)) %1)
   line))

(defn remove-keys [m ks]
  (reduce dissoc m ks))

(defn group-state [{positions :positions} splits]
  [(select-keys positions splits)
   (remove-keys positions splits)])

(defn split-position [[position timelines]]
  (hash-map (dec position) timelines (inc position) timelines))

(defn next-state [state splits]
  (let [[will-split non-splits] (group-state state splits)
        new-states (map split-position will-split)]
    (->State (apply merge-with + non-splits new-states) (+ (:split-count state) (count will-split)))))

(defn solution [file]
  (let [diagram (->> file
                     slurp
                     split-lines
                     (map parse-line))
        start (first (first diagram))
        final-state (reduce next-state (init-state start) (rest diagram))]
    ; (println (:split-count final-state))
    (println (apply + (vals (:positions final-state))))))
