(ns adevent-of-code-2025.day-02 (:gen-class)
    (:require
     [clojure.string :refer [split]]))

(def inclusive-range #(range %1 (inc %2)))

(defn parse-id-range [repr]
  (->> (split repr #"-")
       (map #(Long/parseLong %))
       (apply inclusive-range)))

(defn invalid-id? [id]
  (let [str-id (pr-str id)
        mid (/ (count str-id) 2)]
    (= (subs str-id 0 mid) (subs str-id mid))))

(defn solution [file]
  (->>
   (-> file slurp (split #","))
   (mapcat parse-id-range) (filter invalid-id?) (apply +) println))
