(ns aoc2020.day2
  (:require [clojure.string :as str])
  (:gen-class))

(defn make-rule
  "Takes in a hyphenated range min-max, with the minimum and maximum range being inclusive,
  and a character, letter. This will then return a function which will evaluate if a string
  has the character present a number of times within the range."
  [min-max letter]
  (let [min (Integer/parseInt (first (str/split min-max  #"-")))
        max (Integer/parseInt (second (str/split min-max #"-")))]
    (fn [pass]
      (loop [counter 0
             pos (dec (.length pass))]
        (if (>= pos 0)
          (if (= (.charAt pass pos) letter)
            (if (= counter max)
              false
              (recur (inc counter) (dec pos)))
            (recur counter (dec pos)))
           (>= counter min))))))

(defn make-correct-rule
  "Takes in two 1-indexed indexes, separated by a hyphen, and returns a function that identifies
  if the second parameter, letter, is in only one of those indexes in the string passed to it"
  [places letter]
  (let [i1 (dec (Integer/parseInt (first (str/split places  #"-"))))
        i2 (dec (Integer/parseInt (second (str/split places  #"-"))))]
    (fn [pass]
      (let [letter-1 (.charAt pass i1)
            letter-2 (.charAt pass i2)]
        (if (= letter-1 letter)
          (not (= letter-2 letter))
          (= letter-2 letter))))))

(defn correct-password?
  "Take in a string, break it up where there are spaces, make a rule and apply it to a password."
  [line]
  (let [inputs (str/split line #" ")
        rule (make-rule (first inputs) (first (second inputs)))
        pass (nth inputs 2)]
    (rule pass)))

(defn correct-password-2?
  [line]
  (let [inputs (str/split line #" ")
        rule (make-correct-rule (first inputs) (first (second inputs)))
        pass (nth inputs 2)]
    (rule pass)))


(defn solution-1
  [source]
  (loop [passwords (str/split (slurp source) #"\n")
         correct 0]
    (if (seq passwords)
      (if (check-password (first passwords))
        (recur (rest passwords) (inc correct))
        (recur (rest passwords) correct))
      correct)))

(defn solution-2
  [source]
  (loop [passwords (str/split (slurp source) #"\n")
         correct 0]
    (if (seq passwords)
      (if (correct-password-2? (first passwords))
        (recur (rest passwords) (inc correct))
        (recur (rest passwords) correct))
      correct)))
