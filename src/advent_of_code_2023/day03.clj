; # Day 3
(ns advent-of-code-2023.day03
  (:require [clojure.java.io :as io] ))

(def test-input
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(def real-input
  (slurp (io/resource "day03.txt")))

(def digit?
  #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(def blank?
  #(= \. %))

(defn neighbors
  [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]]
    [(+ x dx) (+ y dy)]))

(def init-parse-state
  {:pos [0 0] ; grid position
   :mode :scan ; mode: scan - looking for start of a number, read - reading a number
   :current-number [] ; during read mode stores digit chars read so far 
   :current-points #{} ; during read mode stores positions read so far 
   :numbers []  ; stores each number and the positions it's digits occupy
   :symbols [] ; stores each symbol and the position it was found at
   })

; State machine transition functions
(defn next-char
  [{[x y] :pos :as state}]
  (assoc state :pos [(inc x) y]))

(defn handle-digit
  [{:keys [pos] :as state} c]
  (-> state
      (assoc :mode :read)
      (update :current-number conj c)
      (update :current-points conj pos)))

(defn term-number
  [{:keys [mode pos current-number current-points] :as state}]
  (if (= :read mode)
    (-> state
        (assoc :mode :scan
               :current-number []
               :current-points #{})
        (update :numbers 
                conj 
                [(parse-long (apply str current-number))
                 current-points]))
    state))

(defn handle-space
  [{:keys [mode current-number pos] :as state} c]
  (if (= :scan mode)
    state
    (term-number state)))

(defn handle-sym
  [{:keys [mode pos] :as state} c]
  (cond-> (update state :symbols conj [c pos])
    (= mode :read) (term-number)))

(defn newline-reset
  [{[x y] :pos mode :mode :as state}]
  (assoc state 
         :mode :scan
         :pos [0 (inc y)]))

(defn handler-char
  [state c]
  (-> (cond (digit? c) (handle-digit state c)
            (= \. c) (handle-space state c)
            :else (handle-sym state c))
      next-char))

; Runs the state machine with the given input 
(defn parse-input
  [input] 
  (->> (reduce (fn [state c]
                 (if (= \newline c)
                   (newline-reset (term-number state))
                   (handler-char state c)))
               init-parse-state 
               input)
       term-number))

(defn find-intersections
  [{:keys [symbols pos->num] :as state}]
  (assoc state
         :intersections
         (map (fn [[sym pos]]
                ; lucky that a symbol doesn't border two identical part numbers 
                [sym (into #{} 
                           (comp (map pos->num)
                                 (remove nil?))
                           (neighbors pos))])
              symbols)))

(defn invert-numbers
  [{:keys [numbers] :as state}]
  (assoc state 
         :pos->num
         (reduce (fn [m [x ps]]
                   (into m (map (fn [p] [p x])) ps))
                 {}
                 numbers)))

(def post-process
  (comp find-intersections
        invert-numbers))

#_(solve-part1 (post-process (parse-input test-input)))
#_(solve-part1 (post-process (parse-input real-input)))
(defn solve-part1
  [{:keys [intersections]}]
  (reduce + (mapcat second intersections)))

#_(solve-part2 (post-process (parse-input test-input)))
#_(solve-part2 (post-process (parse-input real-input)))
(defn solve-part2
  [{:keys [intersections]}]
  (transduce (comp (filter (fn [[sym nums]]
                             (and (= \* sym)
                                  (= 2 (count nums)))))
                   (map (fn [[sym nums]]
                          (apply * nums))))
             +
             intersections))
