(ns advent-of-code-2023.day01
  (:require [clojure.java.io :as io] 
            [clojure.string :as string]
            [clojure.test :refer :all]))

(def again
  {"one" "o1e"
   "two" "t2o"
   "three" "t3e"
   "four" "f4r"
   "five" "f5e"
   "six" "s6x"
   "seven" "s7n"
   "eight" "e8t"
   "nine" "n9e" })

(def process-line 
  (comp
    #(Long/valueOf %)
    (partial apply str) 
    (juxt first last) 
    #(re-seq #"\d" %)))

(def process-line2
  (comp 
    process-line
    (fn [line]
      (reduce (fn [line [k v]]
                (string/replace line k v))
              line
              again))))

(defn solve
  [input process-line-fn]
  (transduce (map process-line-fn)
             +
             (string/split-lines input)))

; part 1
#_(solve (slurp (io/resource "day01.txt")) process-line)
; part 2
#_(solve (slurp (io/resource "day01.txt")) process-line2)
