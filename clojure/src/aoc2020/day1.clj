(ns aoc2020.day1
  (:gen-class))

(defn list-contains?
  "Indicate if value val is present in collection lst"
  [lst val]
  (seq (filter #(= val %) lst)))

(defn solution-1
  "Find two numbers that add to 2020 and return their product. Takes in a string,
  source, which is the path to the input file. Will return nil if there is no pair of
  two numbers in the input that add to 2020"
  [source]
  (loop [parsed '()
         unparsed (clojure.string/split (slurp source) #"\n")]
    (let [x (Integer/parseInt (first unparsed))
          compliment (- 2020 x)]
      (println x)
      (println "compliment: " compliment)
      (if (list-contains? parsed compliment)
        (do
          (println (str "The two entries are " x " and " compliment))
          (* x compliment))
        (recur (conj parsed x) (rest unparsed))))))

(defn two-sum
  "Two-sum calculation. Take in a goal number, goal, and a list of numbers, lst.
  Finds two elements of lst that add to goal, returning the product of these numbers,
  returns nil if there is no pair that adds to goal"
  [goal lst]
  (if (< (count lst) 2)
    nil
    (let [x (first lst)
          compliment (- goal x)]
      (if (list-contains? lst compliment)
        (* x compliment)
        (recur goal (rest lst))))))

(defn solution-2
  "Find three numbers that add to 2020 and return their product. Takes in a string,
  source, which is the path to the input file. Will return nil if there is no set of
  three numbers in the input that add to 2020"
  [source]
  (loop [parsed ' ()
         unparsed (clojure.string/split (slurp source) #"\n")]
    (when (seq unparsed)
      (let [x (Integer/parseInt (first unparsed))
            compliment (- 2020 x)
            possible-pairs (filter #(< % compliment) parsed)]
        (println "Number: " x " Comp: " compliment " Pairs: " possible-pairs)
        (if (empty? possible-pairs)
          (recur (conj parsed x) (rest unparsed))
            (if-let [pair (two-sum compliment possible-pairs)]
              (* pair x)
              (recur (conj parsed x) (rest unparsed))))))))
