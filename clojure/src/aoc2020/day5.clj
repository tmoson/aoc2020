(ns aoc2020.day5
    (:require [clojure.string :as str])
    (:gen-class))

(defn mid-pt
  "Get the number in the middle of `low` and `high`"
  [low high]
  (int (+ low (/ (- high low) 2))))

(defn add-value
  "Adds a value to a BST, or creates a new root node if only a value is given"
  ([value]
    (hash-map :value value :left nil :right nil))
  ([node value]
    (if (nil? node)
      (hash-map :value value :left nil :right nil)
      (let [node-value (:value node)]
        (if (< node-value value)
          (if-let [right-node (:right node)]
            (assoc-in node [:right] (add-value right-node value))
            (assoc-in node [:right] (hash-map :value value :left nil :right nil)))
          (if-let [left-node (:left node)]
            (assoc-in node [:left] (add-value left-node value))
            (assoc-in node [:left] (hash-map :value value :left nil :right nil))))))))

(defn tree-contains?
  "Searches BST `root` (you should pass it the root node) for value `value`"
  [root value]
  (if (nil? root)
    false
    (let [node-value (:value root)]
      (if (= node-value value)
        true
        (if (< node-value value)
          (recur (:right root) value)
          (recur (:left root) value))))))

(defn upper-half
  "Get the beginning and end of the upper half portion of the range `low` to `high`"
  [low high]
  (let [diff (- high low)]
    (if (= diff 1)
      high
      (if (odd? high)
        (list (+ (int (/ (inc diff) 2)) low) high)
        (list (+ (int (/ diff 2)) low) high)))))

(defn lower-half
  "Get the beginning and end of the lower half portion of the range `low` to `high`"
  [low high]
  (let [diff (- high low)]
    (if (= diff 1)
      low
      (list low (+ (int (/ (- high low) 2)) low)))))

(defn get-seat-number
  "Get the number of a seat by taking in the first 7 characters of it"
  ([seat]
    (if (= (first seat) \F)
      (get-seat-number (rest seat) '(0 63))
      (get-seat-number (rest seat) '(64 127))))
  ([seat nums]
    (if (seq seat)
      (if (= (first seat) \F)
        (recur (rest seat) (lower-half (first nums) (second nums)))
        (recur (rest seat) (upper-half (first nums) (second nums))))
      nums)))

(defn get-row-number
  "Get the row number of a seat by taking in the last 3 characters of it"
  ([row]
    (if (= (first row) \R)
      (get-row-number (rest row) '(4 7))
      (get-row-number (rest row) '(0 3))))
  ([row nums]
    (if (seq row)
      (if (= (first row) \R)
        (recur (rest row) (upper-half (first nums) (second nums)))
        (recur (rest row) (lower-half (first nums) (second nums))))
      nums)))

(defn get-seat-id
  "Get the seat-id from a seat's ticket"
  [ticket]
  (+ (* (get-seat-number (take 7 ticket)) 8)
    (get-row-number (drop 7 ticket))))

(defn max-id
  "Recurse through a collection of seats, and return the highest seat-id"
  ([seats]
    (max-id (rest seats) (get-seat-id (first seats))))
  ([seats max]
    (if (seq seats)
      (let [current (get-seat-id (first seats))]
        (if (> current max)
          (recur (rest seats) current)
          (recur (rest seats) max)))
      max)))

(defn binary-tree-seats
  "Takes a path and generates a BST of the seat-ids generated from the lines in the file.
  Alternatively, you can give it a collection of seats, and node to insert from."
  ([path]
    (let [seats (str/split (str/trim (slurp path)) #"\n")]
      (binary-tree-seats (rest seats) (hash-map :value (get-seat-id (first seats)) :left nil :right nil))))
  ([seats root]
    (if (seq seats)
      (recur (rest seats) (add-value root (get-seat-id (first seats))))
      root)))

(defn leftmost
  "Return the value of the leftmost node (smallest value) in the BST, represented by `root`"
  [root]
  (if (nil? (:left root))
    (:value root)
    (recur (:left root))))

(defn rightmost
  "Return the value of the rightmost node (highest value) in the BST, represented by `root`"
  [root]
  (if (nil? (:right root))
    (:value root)
    (recur (:right root))))

(defn missing-seat
  "Returns the number in the collection `nums` that is not present in BST `tree`"
  [nums tree]
  (if (seq nums)
    (if (tree-contains? tree (first nums))
      (recur (rest nums) tree)
      (first nums))
    nil))

(defn find-missing
  "Takes the path to an input file, creates a BST of the seat-ids,
  and searches for the first missing seat id from the input"
  [path]
  (let [seats (binary-tree-seats path)
        lowest (leftmost seats)
        highest (rightmost seats)
        mid (mid-pt lowest highest)
        quarter (mid-pt lowest mid)
        three-quarters (mid-pt mid highest)
        quarters (list (range lowest quarter) (range quarter mid) (range mid three-quarters) (range three-quarters highest))]
    (first (remove nil? (pmap #(missing-seat % seats) quarters)))))

(defn solution-1
  [path]
  (max-id (str/split (str/trim (slurp path)) #"\n")))