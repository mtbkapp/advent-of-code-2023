; # Day 2
; I decided to use the [Instaparse](https://github.com/Engelberg/instaparse) 
; library to do the parsing. Easier than regex in this case? I don't think so. 
; But, it was probably more fun.
(ns advent-of-code-2023.day02
  (:require [clojure.java.io :as io] 
            [clojure.string :as string]
            [clojure.test :refer :all]
            [instaparse.core :as insta]))


(def test-input
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")


(def grammar
  "<L> = Game GameTail* <#'\\s'?>
  <GameTail> = <('\n' | #'\\s')+> Game
  Game = <'Game '> Num <': '> Set SetListTail* 
  <SetListTail> = <'; '> Set
  Num = #'\\d+' 
  Color = 'red' | 'green' | 'blue'
  Set = SetEl SetTail*
  SetEl = Num <' '> Color 
  <SetTail> = <', '> SetEl")

(def parse
  (insta/parser grammar))

(defn parse-set
  [[_ & xs]]
  (reduce (fn [colors [_ [_ n] [_ c]]]
            (assoc colors c (parse-long n)))
            {}
            xs))

(defn extract 
  [[_ [_ id] & sets]]
  {:id (parse-long id)
   :sets (map parse-set sets)})

(defn game-max 
  [{:keys [id sets]}]
  (assoc (apply merge-with max sets)
         :id id))

(defn possible? 
  [q game]
  (and (<= (get game "red" 0) (get q "red" 0))
       (<= (get game "green" 0) (get q "green" 0))
       (<= (get game "blue" 0) (get q "blue" 0))))

(defn filter-possible 
  [q input]
  (into []
        (comp (map extract)
              (map game-max)
              (filter (partial possible? q)))
        input))

(defn part1
  [input q]
  (transduce (comp (map extract)
                   (map game-max)
                   (filter (partial possible? q))
                   (map :id))
             +
             (parse input)))

#_(prn (part1 test-input {"red" 12 "green" 13 "blue" 14}))
#_(prn (part1 (slurp (io/resource "day02.txt")) {"red" 12 "green" 13 "blue" 14}))

(defn calc-power
  [game]
  (* (get game "red" 1)
     (get game "green" 1)
     (get game "blue" 1)))

(defn part2
  [input]
  (transduce (comp (map extract)
                   (map game-max)
                   (map calc-power))
             +
             (parse input)))

#_(prn (part2 test-input))
#_(prn (part2 (slurp (io/resource "day02.txt"))))
