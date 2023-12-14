; # Day 5
(ns advent-of-code-2023.day05
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [instaparse.core :as insta]))

#_((insta/parser grammar) test-input)
#_((insta/parser grammar) real-input)
(def grammar
  "Almanac = Seeds (SectionGap Map)+ <'\\n'>?
  Seeds = <'seeds: '> Seed (<#'\\s+'> Seed)*
  <Seed> = Number 
  <Number> = #'\\d+'
  <SectionGap> = <#'\\n\\n'>
  Map = Name <'-to-'> Name <' map:'> MapLine+
  Name = #'[a-z]+'
  MapLine = <'\\n'> Number <' '> Number <' '> Number")

(def parse-input*
  (insta/parser grammar))

#_(parse-input real-input)
#_(parse-input test-input)
(defn parse-input
  [input]
  (let [[_ [_ & seeds] & ms] (parse-input* input)]
    {:seeds (map parse-long seeds)
     :maps (map (fn [[_ [_ src] [_ dest] & ranges]]
                  {:source src
                   :dest dest
                   :ranges (map (fn [[_ dest-start source-start len]]
                                  {:dest-start (parse-long dest-start)
                                   :source-start (parse-long source-start)
                                   :len (parse-long len)}) 
                                ranges)})
                ms)}))

(def test-input
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(def real-input
  (slurp (io/resource "day05.txt")))
