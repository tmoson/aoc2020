(ns aoc2020.day3
  (:require [clojure.string :as str])
  (:gen-class))

(defn solution-1
  [source]
  (let [nav (str/split (slurp source) #"\n")
        end (count (first nav))]
    (loop [rows nav
           trees 0
           x 0]
      (if (seq rows)
        (do
          (println "Current row: " (first rows))
          (println "x = " x " trees = " trees)
         (if (>= x end)
           (if (= (nth (first rows) (mod x end)) \#)
             (recur (rest rows) (inc trees) (+ x 3))
             (recur (rest rows) trees (+ x 3)))
           (if (= (nth (first rows) x) \#)
             (recur (rest rows) (inc trees) (+ x 3))
             (recur (rest rows) trees (+ x 3)))))
        trees))))

(defn down-one-right-x
  "Go down a single slope, and right amount amt for an input"
  [nav amt]
  (let [end (count (first nav))]
    (loop [rows nav
           trees 0
           x 0]
      (if (seq rows)
        (if (< x end)
          (if (= (nth (first rows) x) \#)
            (recur (rest rows) (inc trees) (+ x amt))
            (recur (rest rows) trees (+ x amt)))
          (if (= (nth (first rows) (mod x end)) \#)
            (recur (rest rows) (inc trees) (+ x amt))
            (recur (rest rows) trees (+ x amt))))
        trees))))

(defn down-two-right-x
  [nav amt]
  (let [end (count (first nav))]
    (loop [rows nav
           trees 0
           x 0]
      (if (seq rows)
        (if (< x end)
          (if (= (nth (first rows) x) \#)
            (recur (rest (rest rows)) (inc trees) (+ x amt))
            (recur (rest (rest rows)) trees (+ x amt)))
          (if (= (nth (first rows) (mod x end)) \#)
            (recur (rest (rest rows)) (inc trees) (+ x amt))
            (recur (rest (rest rows)) trees (+ x amt))))
        trees))))

(defn right-x-down-y
  "Take in a list of strings that make up your map, an amount to increase your x position with,
  and an amount to increase your y position with. Returns the number of trees you'll hit given that trees are
  represented by the \# character on your map."
  [nav amt y]
  (let [end (count (first nav))]
    (loop [rows nav
           trees 0
           x 0]
      (if (seq rows)
        (if (< x end)
          (if (= (nth (first rows) x) \#)
            (recur (drop y rows) (inc trees) (+ x amt))
            (recur (drop y rows) trees (+ x amt)))
          (if (= (nth (first rows) (mod x end)) \#)
            (recur (drop y rows) (inc trees) (+ x amt))
            (recur (drop y rows) trees (+ x amt))))
        trees))))

(defn solution-2
  "Take in a string that is the path to your full map"
  [source]
  (let [nav (str/split (slurp source) #"\n")]
    (reduce * (pmap #(right-x-down-y nav (first %) (second %)) '((1 1) (3 1) (5 1) (7 1) (1 2))))))

