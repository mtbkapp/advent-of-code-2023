; # Day 6
(ns advent-of-code-2023.day06
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]
            [instaparse.core :as insta]))

(def test-input
  [{:t 7 :rec 9}
   {:t 15 :rec 40}
   {:t 30 :rec 200}])

(def real-input 
  [{:t 41 :rec 214}
   {:t 96 :rec 1789}
   {:t 88 :rec 1127}
   {:t 94 :rec 1055}])

; dist(total, hold) = hold * (total - hold)
; velocity = hold time
; distance = velocity * moving time 
; moving time = total time - hold -time
; distance = hold time * (total time - hold-time)
(defn dist
  [total-t hold-t]
  (* hold-t (- total-t hold-t)))

(defn ways-to-win
  [t rec]
  (into []
        (comp 
          (map (partial dist t))
          (filter #(< rec %)))
        (range (inc t))))

#_(prn (solve-part1 test-input))
#_(prn (solve-part1 real-input))
(defn solve-part1
  [input]
  (transduce (comp
               (map (fn [{:keys [t rec]}]
                      (ways-to-win t rec)))
               (map count))
             *
             input))

(def test-input2
  {:t 71530
   :rec 940200})

(def real-input2
  {:t 41968894 
   :rec 214178911271055})

; A bit of math is needed to solve part 2 since the numbers are so big. 
; The distance equation given the total time (t_total) and hold time (t_hold) 
; is:
;
; dist = velocity * t_moving 
; velocity = t_hold
; t_moving = t_total - t_hold
; dist = t_hold * (t_total - t_hold)
; dist = -t_hold^2 + (t_hold * t_total)
;
; so t_total is a given and t_hold is variable.
; let x = t_hold 
; let T = t_total - given in the input 
; let y = dist 
; y = -x^2 + Tx
;
; Plotting this equation one gets an inverted parabola. The number of integer 
; x's that result in y > given record distance (d_record) is the number of ways 
; to beat the record. So what needs to be calculated are the two places where 
; the plot crosses the line y = ; d_record. Rewriting results in:
; let R = d_record
; R = -x^2 + Tx
; 0 = -x^2 + Tx - R
; 
; Now the quadratic equation can be used where a = -1, b = T, and C = -R.
; x_0 = (-T + sqrt(T^2 - 4R)) / -2
; x_1 = (-T - sqrt(T^2 - 4R)) / -2

; To get the number of ways to beat the record:
; ceiling(x_1) - ceiling(x_0)
;
; I thought that because java.lang.Math returns a 64 bit double there might be
; precision issues and I'd have to get into Java's arbitrary precision numbers.
; But, at least for my input, it works with regular 64 bit doubles.

#_(prn (solve-part2 test-input2))
#_(prn (solve-part2 real-input2))
(defn solve-part2
  [{:keys [t rec]}]
  (let [sqrt (Math/sqrt (- (* t t) (* 4 rec)))
        start (Math/ceil (/ (+ (- t) sqrt) -2))
        end (Math/ceil (/ (- (- t) sqrt) -2))]
    (- end start)))

