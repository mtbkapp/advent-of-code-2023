; # Day 5
(ns advent-of-code-2023.day05
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]
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

(defn range-max
  [start len]
  (+ start len -1))

(defn range-from-end
  [start end]
  {:start start
   :end end
   :len (- (inc end) start)})

(defn range-from-len
  [start len]
  {:start start 
   :end (+ start len -1)
   :len len})

#_(clojure.pprint/pprint (parse-input2 test-input))
(defn parse-input2
  [input]
  (let [[_ [_ & seeds] & ms] (parse-input* input)]
    {:seeds (into [] 
                  (comp 
                    (map parse-long) 
                    (partition-all 2)
                    (map (fn [[s l]]
                           (range-from-len s l)))) 
                  seeds)
     :maps (map (fn [[_ [_ src] [_ dest] & ranges]]
                  {:source src
                   :dest dest
                   :ranges (map (fn [[_ dest-start source-start len]]
                                  (let [s (parse-long source-start)
                                        d (parse-long dest-start)
                                        l (parse-long len)]
                                    {:source (range-from-len s l)
                                     :dest (range-from-len d l) 
                                     :diff (- d s)
                                     :len l})) 
                                ranges)})
                ms)}))

(defn split-seed-range
  [seed-range mapping-range]
  (let [{seed-start :start seed-end :end} seed-range
        {{mapping-start :start mapping-end :end} :source} mapping-range]
    (cond
      ; seed-range is contained within mapping-range
      (and (<= mapping-start seed-start mapping-end)
           (<= mapping-start seed-end mapping-end))
      {:in seed-range
       :out []}

      ; mapping-range is contained within seed-range
      (and (< seed-start mapping-start seed-end)
           (< seed-start mapping-end seed-end))
      {:in (range-from-end mapping-start mapping-end)
       :out [(range-from-end seed-start (dec mapping-start))
             (range-from-end (inc mapping-end) seed-end)]}

      ; seed-range end overlaps with mapping-range start
      (<= mapping-start seed-end mapping-end)
      {:in (range-from-end mapping-start seed-end) 
       :out [(range-from-end seed-start (dec mapping-start))]}

      ; seed-range start overlaps with mapping-range end
      (<= mapping-start seed-start mapping-end)
      {:in (range-from-end seed-start mapping-end)
       :out [(range-from-end (inc mapping-end) seed-end)]}

      ; no overlap
      :else {:in nil :out [seed-range]})))

(defn map-seed-range
  [{:keys [diff] :as mapping-range} seed-range]
  (-> seed-range
      (update :start + diff)
      (update :end + diff)))

(defn push-range
  [seed-ranges mapping-range]
  (reduce (fn [acc seed-range]
            (let [{:keys [in out]} (split-seed-range seed-range mapping-range)]
              (cond-> (update acc :out into out)
                (some? in) (update :in 
                                   conj 
                                   (map-seed-range mapping-range in)))))
          {:in [] :out []}
          seed-ranges))

(defn push*
  [seed-ranges {:keys [ranges] :as mapping}]
  (reduce (fn [{:keys [not-mapped] :as acc} mapping-range]
            (let [{:keys [in out]} (push-range not-mapped mapping-range)]
              (-> acc
                  (update :mapped into in)
                  (assoc :not-mapped out))))
          {:mapped [] :not-mapped seed-ranges}
          ranges))

(defn push
  [seed-range mappings]
  (reduce (fn [last-seed-ranges mapping]
            (let [{:keys [mapped not-mapped]} (push* last-seed-ranges mapping)]
              (into mapped not-mapped)))
          [seed-range]
          mappings))

#_(solve-part2 (parse-input2 test-input))
#_(prn (solve-part2 (parse-input2 real-input)))
(defn solve-part2
  [{:keys [seeds maps]}]
  (transduce (comp
               (mapcat #(push % maps))
               (map :start))
             min 
             Long/MAX_VALUE
             seeds))

