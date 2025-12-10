(ns advent-of-code-2025.day-10 (:gen-class)
    (:require
     [clojure.string :refer [split split-lines]])
    (:import
     [clojure.lang PersistentQueue]))

(defn parse-indicator [s]
  (map
   #(case %
      \# :on
      \. :off)
   (butlast (rest s))))

(defn parse-button [s]
  (map
   #(Integer/parseInt %)
   (split (subs s 1 (dec (count s))) #",")))

(defn parse-line [line]
  (let [elements (split line #" ")]
    {:indicator (parse-indicator (first elements))
     :buttons   (map parse-button (butlast (rest elements)))}))

(defn toggle [state n]
  (concat
   (take n state)
   (list (if (= :on (nth state n)) :off :on))
   (drop (inc n) state)))

(defn press [state button]
  (reduce toggle state button))

(defn act
  ([machine] (act machine
                  (conj PersistentQueue/EMPTY (vector (repeat (count (:indicator machine)) :off) 0))
                  #{}))
  ([machine queue visited]
   (let [[current n] (peek queue)
         new-states (remove visited (map #(press current %) (:buttons machine)))]
     (cond
       (visited current) (recur machine (pop queue) visited)
       (some #{(:indicator machine)} new-states) (inc n)
       :else (recur
              machine
              (apply conj (pop queue) (map #(vector % (inc n)) new-states))
              (conj visited current))))))

(defn solution [file]
  (->> file
       slurp
       split-lines
       (map parse-line)
       (map act)
       (apply +)
       println))
