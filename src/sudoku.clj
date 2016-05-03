(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def nrc-board-2567
  (board [[0 9 0 0 7 6 0 0 0]
          [0 0 3 0 2 0 6 0 1]
          [0 0 2 1 0 5 0 0 0]
          [0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 7 0 0 0]
          [0 0 0 3 0 8 9 0 0]
          [0 0 0 2 0 3 0 0 0]
          [0 0 4 5 0 0 0 2 0]
          [0 0 6 0 0 0 4 0 0]]))


(def nrc-board-2568
  (board [[0 0 0 0 0 9 4 0 0]
          [0 0 0 0 5 0 0 3 0]
          [0 4 3 2 0 0 0 0 1]
          [0 0 0 0 0 0 0 6 0]
          [0 0 0 0 0 0 0 1 0]
          [0 5 0 9 0 0 0 4 8]
          [0 0 4 1 6 0 0 0 0]
          [2 3 0 0 7 0 0 0 0]
          [5 0 0 0 0 0 0 0 0]]))

(def nrc-board-2569
  (board [[0 1 0 0 0 2 0 0 5]
          [0 0 0 0 0 5 0 0 0]
          [0 5 0 0 0 8 0 0 0]
          [0 0 0 3 0 0 0 0 0]
          [0 0 0 0 0 7 6 0 0]
          [0 8 0 0 9 0 1 0 0]
          [2 0 3 7 0 0 4 8 0]
          [0 0 0 4 0 0 0 0 0]
          [0 0 0 0 8 0 9 0 6]]))

(def nrc-board-2570
  (board [[1 0 0 0 0 0 0 0 0]
          [0 0 0 0 5 0 6 0 0]
          [0 3 0 1 0 0 0 0 0]
          [2 0 0 0 3 9 0 0 0]
          [0 7 0 2 0 0 0 0 0]
          [0 0 5 0 0 0 9 0 0]
          [0 0 0 0 0 1 0 7 4]
          [0 0 2 0 0 0 0 3 0]
          [8 9 0 0 7 0 0 5 0]]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def nrcblockcoords #{1 2 3 5 6 7})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (map (fn [x] (value-at board [row x])) (range 0 9)))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (map (fn [x] (value-at board [x col])) (range 0 9)))))

(defn coord-pairs [coord-sequence]
  (for [row coord-sequence col coord-sequence] [row col]))

(defn top-left [coord]
  (let [[row col] coord]
    [(* (int (/ row 3)) 3) (* (int (/ col 3)) 3)]))

(defn five-or-one [x]
  (if (< x 4)
    1
    5))

(defn nrc-top-left [coord]
    (let [[row col] coord]
      [(five-or-one row) (five-or-one col)]))

(defn block-coords [coord]
  (let [[top left] (top-left coord)]
    (for [row [top (inc top) (inc (inc top))] col [left (inc left) (inc (inc left))]] [row col])))

(defn nrc-block-coords [coord]
  (let [[top left] (nrc-top-left coord)]
    (for [row [top (inc top) (inc (inc top))] col [left (inc left) (inc (inc left))]] [row col])))

(defn block-values [board coord]
  (set (map (fn [x] (value-at board x)) (block-coords coord))))

(defn in-nrc-block? [coord]
  (let [[row col] coord]
    (and (contains? nrcblockcoords row) (contains? nrcblockcoords col))))

(defn nrc-block-values [board coord]
  (if (in-nrc-block? coord)
    (set (map (fn [x] (value-at board x)) (nrc-block-coords coord)))
    #{}))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn valid-nrc-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord) (nrc-block-values board coord)))))

(defn filled? [board]
  (not (contains? (set (map (fn [x] (has-value? board x)) (coord-pairs [0 1 2 3 4 5 6 7 8]))) false)))

(defn rows [board]
  (map (fn [x] (row-values board [x 0])) (range 0 9)))

(defn valid-rows? [board]
  (every? (fn [x] (empty? (set/difference all-values x))) (rows board)))

(defn cols [board]
  (map (fn [x] (col-values board [0 x])) (range 0 9)))

(defn valid-cols? [board]
  (every? (fn [x] (empty? (set/difference all-values x))) (cols board)))

(defn blocks [board]
  (map (fn [coord] (block-values board coord)) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? (fn [x] (empty? (set/difference all-values x))) (blocks board)))

(defn nrc-blocks [board]
  (map (fn [coord] (nrc-block-values board coord)) (coord-pairs [1 5])))

(defn valid-nrc-blocks? [board]
  (every? (fn [x] (empty? (set/difference all-values x))) (nrc-blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn valid-nrc-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board) (valid-nrc-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [all-pairs (coord-pairs [0 1 2 3 4 5 6 7 8])]
    (if (not (has-value? board (first all-pairs)))
      (first all-pairs)
      (recur (rest all-pairs)))))

(defn solve-helper [current-board]
  (if (filled? current-board)
    (if (valid-solution? current-board)
      [current-board]
      nil)
    (let [empty-location (find-empty-point current-board)]
      (for [valid-value (valid-values-for current-board empty-location)
            solution (solve-helper (set-value-at current-board empty-location valid-value))] solution)
      ))
  )

(defn solve-nrc-helper [current-board]
  (if (filled? current-board)
    (if (valid-nrc-solution? current-board)
      [current-board]
      nil)
    (let [empty-location (find-empty-point current-board)]
      (for [valid-value (valid-nrc-values-for current-board empty-location)
            solution (solve-nrc-helper (set-value-at current-board empty-location valid-value))] solution)
      ))
  )

(defn solve [board]
  (first (solve-helper board)))

(defn solve-nrc [board]
  (first (solve-nrc-helper board)))

