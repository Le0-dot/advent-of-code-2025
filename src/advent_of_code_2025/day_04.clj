(ns advent-of-code-2025.day-04 (:gen-class)
    (:require
     [clojure.string :refer [split-lines]]))

(defrecord Position [state idx])

(defn empty-position? [position] (= :empty (:state position)))

(defn parse-line [line-idx line]
  (map-indexed
   #(->Position
     (case %2 \. :empty \@ :roll)
     (+ (* (count line) line-idx) %1))
   line))

(defn pad-with [padding coll] (concat [padding] coll [padding]))

(defn pad-grid-with-empty [coll]
  (let [empty-position (->Position :empty -1)]
    (->> coll
         (map #(pad-with empty-position %))
         (pad-with (-> coll first count inc inc (repeat empty-position))))))

(defn vertical-partitions [n coll]
  (->> coll
       (map #(partition n 1 %))
       (apply map concat)))

(defn grid-partitions [n coll]
  (->> coll
       (partition n 1)
       (mapcat #(vertical-partitions n %))))

(defn middle [coll] (nth coll (quot (count coll) 2)))

(defn not-empty-middle? [coll]
  (-> coll middle empty-position? not))

(defn accesssible? [coll]
  (->> coll
       (filter #(not (empty-position? %)))
       count
       (>= 4)))

(defn remove-positions [grid coll]
  (let [empty-position (->Position :empty -1)
        indecies (set (map :idx coll))
        remove-from-row (fn [row] (map #(if (contains? indecies (:idx %)) empty-position %) row))]
    (map remove-from-row grid)))

(defn count-removable [grid]
  (let [removable (->> grid
                       (grid-partitions 3)
                       (filter not-empty-middle?)
                       (filter accesssible?)
                       (map middle))
        removable-count (count removable)]
    (+ removable-count
       (if (pos? removable-count)
         (count-removable (remove-positions grid removable))
         0))))

(defn solution [file]
  (->> file
       slurp
       split-lines
       (map-indexed parse-line)
       pad-grid-with-empty
       count-removable
       println))
