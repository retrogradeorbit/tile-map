(ns tile-map.line
  (:require [infinitelives.utils.console :refer [log]]))


(defn octant-of [x0 y0 x1 y1]
  (cond
    (and (< x0 x1)
         (< y0 y1)
         (> (- x1 x0) (- y1 y0)))
    0

    (and (< x0 x1)
         (< y0 y1)
         (<= (- x1 x0) (- y1 y0)))
    1

    (and (<= x1 x0)
         (< y0 y1)
         (< (- x0 x1) (- y1 y0)))
    2

    (and (<= x1 x0)
         (< y0 y1)
         (>= (- x0 x1) (- y1 y0)))
    3

    (and (<= x1 x0)
         (<= y1 y0)
         (> (- x0 x1) (- y0 y1)))
    4

    (and (<= x1 x0)
         (<= y1 y0)
         (<= (- x0 x1) (- y0 y1)))
    5

    (and (< x0 x1)
         (<= y1 y0)
         (< (- x1 x0) (- y0 y1)))
    6

    (and (< x0 x1)
         (<= y1 y0)
         (>= (- x1 x0) (- y0 y1)))
    7))

(defn to-zero-octant [octant x y]
  (case octant
    0 [x y]
    1 [y x]
    2 [y (- x)]
    3 [(- x) y]
    4 [(- x) (- y)]
    5 [(- y) (- x)]
    6 [(- y) x]
    7 [x (- y)]))

(defn from-zero-octant [octant x y]
  (case octant
    0 [x y]
    1 [y x]
    2 [(- y) x]
    3 [(- x) y]
    4 [(- x) (- y)]
    5 [(- y) (- x)]
    6 [y (- x)]
    7 [x (- y)]))

(defn bresenham [x0 y0 x1 y1]
  (let [octant (octant-of x0 y0 x1 y1)
        [x0 y0] (to-zero-octant octant x0 y0)
        [x1 y1] (to-zero-octant octant x1 y1)]
    (for [x (range x0 (inc x1))]
      (from-zero-octant
       octant x (+ y0
                   (* (- x x0)
                      (/ (- y1 y0)
                         (- x1 x0))))))))

(defn intify [s]
  (for [[x y] s]
    [(int (+ 0.5 x))
     (int (+ 0.5 y))]))

(let [[x0 y0 x1 y1] [-10 5 0 5]]
  (log (str (intify (bresenham x0 y0 x1 y1))))
  (log (str (octant-of x0 y0 x1 y1))))
