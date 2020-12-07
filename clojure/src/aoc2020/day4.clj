(ns aoc2020.day4
    (:require [clojure.string :as str])
    (:gen-class))

;; These are all the valid fields
(def fields '(:byr :iyr :eyr :hgt :hcl :ecl :pid))
(def colors '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))
(defmacro valid-birth-year? [year] `(and (> ~year 1919) (< ~year 2003)))
(defmacro valid-issue-year? [year] `(and (> ~year 2009) (< ~year 2021)))
(defmacro valid-cm? [height] `(and (> ~height 149) (< ~height 194)))
(defmacro valid-in? [height] `(and (> ~height 58) (< ~height 77)))
(defmacro valid-ex-year? [year] `(and (> ~year 2019) (< ~year 2031)))

(defn list-contains?
  "Check if collection `lst` contains value `val`, using the *=* operator"
  [lst val]
  (if (seq lst)
    (if (= (first lst) val)
      true
      (recur (rest lst) val))
    nil))

(defn mapify
  "Create a map, representing a passport, by iterating through
  a collection of  `:` separated strings, where the first value is
  the keyword, and the second is the value"
  [string-list]
  (loop [lst string-list
         result nil]
    (if (seq lst)
      (let [current (str/split (first lst) #":")]
        (recur (rest lst) (assoc result (keyword (first current)) (second current))))
      result)))

(defn valid-hgt?
  "Check if the height passed in is valid"
  [height-val]
  (if (nil? height-val)
    false
    (if (re-find #"cm" height-val)
      (valid-cm? (Integer/parseInt (re-find #"\d+" height-val)))
      (if (re-find #"in" height-val)
        (valid-in? (Integer/parseInt (re-find #"\d+" height-val)))
        false))))

(defn valid-byr?
  "Check if the birth year entered is valid"
  [year]
  (if (= (count year) 4)
    (if-let [parsed-year (re-matches #"\d+" year)]
      (valid-birth-year? (Integer/parseInt parsed-year))
      false)
    false))

(defn valid-isd?
  "Check if the issue year is valid"
  [year]
  (if (= (count year) 4)
    (if-let [parsed-year (re-matches #"\d+" year)]
      (valid-issue-year? (Integer/parseInt parsed-year))
      false)
    false))

(defn valid-eyr?
  "Check if the expiration year is valid"
  [year]
  (if (= (count year) 4)
    (if-let [parsed-year (re-matches #"\d+" year)]
      (valid-ex-year? (Integer/parseInt parsed-year))
      false)
    false))

(defn valid-hcl?
  "Check if the haircolor is valid"
  [color]
  (if (nil? color)
    false
    (re-matches #"#([a-f|0-9][a-f|0-9][a-f|0-9][a-f|0-9][a-f|0-9][a-f|0-9])" color)))

(defn valid-pid?
  "Check if the passport-id is valid"
  [pid]
  (if (nil? pid)
    false 
    (re-matches #"\d\d\d\d\d\d\d\d\d" pid)))

(defn valid-ecl?
  "Check if the eye color is in the list of valid eye colors"
  [color]
  (list-contains? colors color))

;; A collection of validation functions that go with the keys that are in the passport
(def section-functions (hash-map :eyr valid-eyr?, :ecl valid-ecl?, :pid valid-pid?, :hcl valid-hcl?, :iyr valid-isd?, :byr valid-byr?, :hgt valid-hgt?))

(defn valid-passport?
  "Validate all of the important portions of the passport"
  [passport]
  (loop [sections fields]
    (if (seq sections)
      (let [current (first sections)]
        (if ((get section-functions current) (get passport current))
          (recur (rest sections))
          false))
      true)))

(defn valid?
  "Check if a passport has all of the fields needed"
  [passport]
  (loop [sections fields]
    (if (seq sections)
      (if (get passport (first sections))
        (recur (rest sections))
        false)
      true)))

(defn count-valid
  "Count the number of passports that have all of the necessary fields"
  ([passports]
    (if (valid? (first passports))
      (count-valid (rest passports) 1)
      (count-valid (rest passports) 0)))
  ([passports valid]
    (if (seq passports)
      (if (valid? (first passports))
        (recur (rest passports) (inc valid))
        (recur (rest passports) valid))
      valid)))

(defn solution-1
  "Solve the first portion of the challenge"
  [path]
  (let [src (str/split (slurp path) #"\n\n")
        passports (map (fn [pass] (mapify (str/split pass #"([\s|\n])"))) src)]
    (count-valid passports)))

(defn solution-2
  "Solve the second portion of the challenge"
  [path]
  (let [src (str/split (slurp path) #"\n\n")
        passports (map (fn [pass] (mapify (str/split pass #"([\s|\n])"))) src)]
    (loop [need-validating passports
           valid 0]
      (if (seq need-validating)
        (if (valid-passport? (first need-validating))
          (recur (rest need-validating) (inc valid))
          (recur (rest need-validating) valid))
        valid))))
