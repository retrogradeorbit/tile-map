(ns tile-map.line
  (:require [infinitelives.utils.console :refer [log]]
            [infinitelives.utils.vec2 :as vec2]
            [cljs.core.match :refer-macros [match]]
))


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

(defn cover-all [s]
  (apply
   concat
   (for [[x y] s]
     (cond
       (= 0.5 (mod x 1)) [[(int x) y] [(inc (int x)) y]]
       (= 0.5 (mod y 1)) [[x (int y)] [x (inc (int y))]]
       :default [[x y]]))))

(defn intersect-x [x0 y0 x1 y1 y]
  (+ x0
     (* (- y y0)
        (/ (- x1 x0)
           (- y1 y0)))))

(defn intersect-y [x0 y0 x1 y1 x]
  (+ y0
     (* (- x x0)
        (/ (- y1 y0)
           (- x1 x0)))))

(defn cell-coverage [x0 y0 x1 y1
                     x-fn y-fn
                     dy-v-fn
                     dx-h-fn]
  (loop [x (int x0) y (int y0) s [[x y]]]
    (if (and (= x (Math/floor x1)) (= y (Math/floor y1)))
      s
      (let [top-bottom-x (intersect-x x0 y0 x1 y1 (y-fn y))
            left-right-y (intersect-y x0 y0 x1 y1 (x-fn x))]
        (cond
          ;; cuts top/bottom
          (<= x top-bottom-x (inc x))
          (recur x (dy-v-fn y)
                 (conj s [x (dy-v-fn y)]))

          ;; cuts left/right
          (<= y left-right-y (inc y))
          (recur (dx-h-fn x) y
                 (conj s [(dx-h-fn x) y]))

          ;; cuts perfect diagonal
          (and (= top-bottom-x (dx-h-fn x))
               (= left-right-y (dy-v-fn y)))
          (recur (dx-h-fn x) (dy-v-fn y)
                 (conj s
                       [x (dy-v-fn y)]
                       [(dx-h-fn x) y]
                       [(dx-h-fn x) (dy-v-fn y)]))

          :default (assert false "cell-coverage fatal error!"))))))

(defn cell-coverage-line [x0 y0 x1 y1 xtest ytest dx-fn dy-fn]
  (loop [x (int x0)
         y (int y0)
         s [[x y]]]
    (if (or (and xtest (= x (Math/floor x1)))
            (and ytest (= y (Math/floor y1))))
      s
      (cond
        xtest
        (let [right-y (intersect-y x0 y0 x1 y1 (inc x))]
          (assert (<= y right-y (inc y)) "intersection out of range")
          (recur (dx-fn x) (dy-fn y)
                 (conj s [(dx-fn x) (dy-fn y)])))

        ytest
        (let [top-x (intersect-x x0 y0 x1 y1 (inc y))]
          (assert (<= x top-x (inc x)) "intersection out of range")
          (recur (dx-fn x) (dy-fn y)
                 (conj s [(dx-fn x) (dy-fn y)])))))))

(defn all-covered [x0 y0 x1 y1]
  (match [(Math/sign (- x1 x0)) (Math/sign (- y1 y0))]

         ;; diagonals
         [1 1] (cell-coverage x0 y0 x1 y1 inc inc inc inc)
         [1 -1] (cell-coverage x0 y0 x1 y1 inc identity dec inc)
         [-1 1] (cell-coverage x0 y0 x1 y1 identity inc inc dec)
         [-1 -1] (cell-coverage x0 y0 x1 y1 identity identity dec dec)

         ;; compass directions
         [1 _] (cell-coverage-line x0 y0 x1 y1 true false inc identity)
         [-1 _] (cell-coverage-line x0 y0 x1 y1 true false dec identity)
         [_ -1] (cell-coverage-line x0 y0 x1 y1 false true identity dec)
         [_ 1] (cell-coverage-line x0 y0 x1 y1 false true identity inc)))
