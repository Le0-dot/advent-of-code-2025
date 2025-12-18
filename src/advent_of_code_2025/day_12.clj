(ns advent-of-code-2025.day-12 (:gen-class)
    (:require
     [clojure.set :refer [map-invert]]
     [clojure.string :refer [split-lines]]))

(defn debug [x] (println x) x)

(defn debug-region [region]
  (doseq [line region]
    (println (apply str (map {:empty \., :taken \#} line))))
  (println)
  region)

(defn map-grid [f & colls]
  (apply map #(apply map f %&) colls))

(defn parse-shape [lines] (map-grid {\. :empty, \# :taken} (rest lines)))

(defn rotate-shape [shape] (apply map list (map reverse shape)))

(defn flip-shape [shape]
  (map reverse shape))

(defn variations [shape]
  (set [(map seq shape)
        (flip-shape shape)
        (rotate-shape shape)
        (flip-shape (rotate-shape shape))
        (rotate-shape (rotate-shape shape))
        (flip-shape (rotate-shape (rotate-shape shape)))
        (rotate-shape (rotate-shape (rotate-shape shape)))
        (flip-shape (rotate-shape (rotate-shape (rotate-shape shape))))]))

(defn init-region [x y] (repeat y (repeat x :empty)))

(defn parse-region [line]
  (let [[[x y] indecies] (->> line (re-seq #"\d+") (split-at 2))]
    [(init-region (Integer/parseInt x) (Integer/parseInt y)),
     (vec (map #(Integer/parseInt %) indecies))]))

(defn fits? [subregion shape]
  (let [proj {:empty true, :taken false}]
    (every?
     #(every? identity %)
     (map-grid
      #(or %1 %2)
      (map-grid proj subregion)
      (map-grid proj shape)))))

(defn place-shape [subregion shape]
  (let [proj {:empty false, :taken true}]
    (map-grid
     #((map-invert proj) (or %1 %2))
     (map-grid proj subregion)
     (map-grid proj shape))))

(defn take-subregion [region x y]
  (->> region
       (drop y)
       (take 3)
       (map #(->> % (drop x) (take 3)))))

(defn replace-subregion [region x y subregion]
  (let [before-rows (take y region)
        after-rows (drop (+ y 3) region)
        to-replace-rows (->> region (drop y) (take 3))]
    (concat
     before-rows
     (map #(concat (take x %1) %2 (drop (+ x 3) %1)) to-replace-rows subregion)
     after-rows)))

(defn try-place [region shape]
  (let [h (count region)
        w (count (first region))
        coords (for [y (reverse (range (- h 2)))
                     x (range (- w 2))]
                 [x y])
        found (some #(when (fits? (apply take-subregion region %) shape) %) coords)]
    (when (some? found)
      (let [[x y] found
            subregion (take-subregion region x y)
            updated (place-shape subregion shape)]
        (replace-subregion region x y updated)))))

(defn choose-shapes [shapes shapes-count]
  (->> shapes-count
       (map #(when (pos? %3) [%1 %2]) (range (count shapes)) shapes)
       (filter some?)
       (map #(vector (second %) (update shapes-count (first %) dec)))))

(defn flatten-branch [[shape state]]
  (map #(vector %1 state) shape))

(defn fill-region [shapes region shapes-count]
  (let [branches (mapcat flatten-branch (choose-shapes shapes shapes-count))
        regions (->> branches (map first) (map #(try-place region %)) (remove nil?))
        states (map second branches)]
    (cond
      (empty? branches) true
      (empty? regions) false
      :else (some true? (map #(fill-region shapes %1 %2) regions states)))))

(defn solution [file]
  (let [lines (->> file
                   slurp
                   split-lines
                   (partition-by empty?)
                   (remove #(= 1 (count %))))
        shapes (->> lines butlast (map parse-shape) (map variations))
        regions (->> lines last (map parse-region))]
    (->> regions
         (map #(apply fill-region shapes %))
         (filter identity)
         count
         println)))
