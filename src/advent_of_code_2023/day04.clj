; # Day 4
(ns advent-of-code-2023.day04
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]))

(def test-input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(def real-input 
  (slurp (io/resource "day04.txt")))

(defn read-nums
  [s]
  (into #{}
        (map parse-long)
        (re-seq #"\d+" s)))

(defn read-ln 
  [line]
  (let [[card winners nums] (string/split line #"\||\:")]
    {:card-num (->> card (re-seq #"\d+") first parse-long)
     :winners (read-nums winners) 
     :nums (read-nums nums)}))

(defn score-card
  [{:keys [winners nums]}]
  (let [c (count (sets/intersection winners nums))]
    (if (< 0 c)
      (long (Math/pow 2 (dec c)))
      0)))

(defn read-input
  [input]
  (map read-ln (string/split-lines input)))

#_(solve-part1 (read-input test-input))
#_(prn (solve-part1 (read-input real-input)))
(defn solve-part1
  [cards]
  (transduce (map score-card) + cards))

(defn count-wins
  [{:keys [winners nums]}]
  (count (sets/intersection winners nums)))

(defn get-copies 
  [cards {:keys [card-num win-count]}]
  (subvec cards card-num (+ card-num win-count)))

#_(solve-part2 (read-input test-input))
#_(prn (solve-part2 (read-input real-input))) ; 6050769
(defn solve-part2
  [input]
  (let [cards (mapv #(assoc % :win-count (count-wins %)) input)
        q (into (clojure.lang.PersistentQueue/EMPTY) cards)]
    (loop [q q c 0]
      (assert (< c 10000000) "max iterations!")
      (if (empty? q)
        c
        (recur (into (pop q) (get-copies cards (peek q)))
               (inc c))))))

