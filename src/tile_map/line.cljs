(ns tile-map.line
  (:require [infinitelives.utils.console :refer [log]]
            [infinitelives.utils.vec2 :as vec2]))


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

(defn quadrant-of-angle [x0 y0 x1 y1]
  (cond
    (and (< x0 x1) (< y0 y1))
    0

    (and (<= x1 x0) (< y0 y1))
    1

    (and (<= x1 x0) (<= y1 y0))
    2

    (and (< x0 x1) (<= y1 y0))
    3)
  )

(defn to-zero-quadrant [quad x y]
  (case quad
    0 [x y]
    1 [(- x) y]
    2 [(- x) (- y)]
    3 [x (- y)]))

(def from-zero-quadrant to-zero-quadrant)

(defn all-covered [x0 y0 x1 y1]
  (log "a-cov" x0 y0 x1 y1)
  (let [quadrant (quadrant-of-angle x0 y0 x1 y1)
        [x0 y0] (to-zero-quadrant quadrant x0 y0)
        [x1 y1] (to-zero-quadrant quadrant x1 y1)]
    (loop [x (int x0) y (int y0) s [(from-zero-quadrant quadrant (int x0) (int y0))]]
      (log "AC" x y (str s))
      (if (and (= x (int x1)) (= y (int y1)))
        s

        (let [bottom-x (intersect-x x0 y0 x1 y1 (inc y))
              right-y (intersect-y x0 y0 x1 y1 (inc x))
              ]
          (log "=>" x bottom-x (inc x) y right-y (inc y))
          (cond (<= x bottom-x (inc x))
                ;; cuts bottom
                (recur x (inc y)
                       (conj s (from-zero-quadrant quadrant x (inc y))))

                ;; cuts right
                (<= y right-y (inc y))
                (recur (inc x) y
                       (conj s (from-zero-quadrant quadrant (inc x) y)))

                (and (= bottom-x (inc x))
                     (= right-y (inc y)))
                (recur (inc x) (inc y)
                       (conj s
                             (from-zero-quadrant quadrant x (inc y))
                             (from-zero-quadrant quadrant (inc x) y)
                             (from-zero-quadrant quadrant (inc x) (inc y))))

                :default :foo
                )


          )))
    )
  )

(log (str (all-covered 6.7 4.6 7.002179031074047 4.591119113750755)))






#_ (let [[x0 y0 x1 y1] [0 0 3 9]]
  #_ (log (str (bresenham x0 y0 x1 y1)))
  #_ (log (str (octant-of x0 y0 x1 y1)))
  #_ (log (str (intify (cover-all (bresenham x0 y0 x1 y1)))))

  (log (str (intersect-x 1.9 0.5 2.5 2.3 2))
       (str (intersect-y 1.9 0.5 2.5 2.3 2)))
)
