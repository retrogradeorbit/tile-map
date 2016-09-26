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
  (let [
        dx (- x1 x0)
        dy (- y1 y0)

        down? (pos? dy)
        up? (neg? dy)
        horiz? (zero? dy)

        right? (pos? dx)
        left? (neg? dx)
        vert? (zero? dx)


        ]
    (cond
      (and right? down?)
      (loop [x (int x0)
             y (int y0)
             s [[x y]]]
        (if (and (= x (Math/floor x1)) (= y (Math/floor y1)))
          s

          (let [bottom-x (intersect-x x0 y0 x1 y1 (inc y))
                right-y (intersect-y x0 y0 x1 y1 (inc x))
                ]
            (cond (<= x bottom-x (inc x))
                  ;; cuts bottom
                  (recur x (inc y)
                         (conj s [x (inc y)]))

                  ;; cuts right
                  (<= y right-y (inc y))
                  (recur (inc x) y
                         (conj s [(inc x) y]))

                  (and (= bottom-x (inc x))
                       (= right-y (inc y)))
                  (recur (inc x) (inc y)
                         (conj s
                               [x (inc y)]
                               [(inc x) y]
                               [(inc x) (inc y)]))

                  :default :down-right))))

      (and right? up?)
      (loop [x (int x0)
             y (int y0)
             s [[x y]]]
        (if (and (= x (Math/floor x1)) (= y (Math/floor y1)))
          s

          (let [top-x (intersect-x x0 y0 x1 y1 y)
                right-y (intersect-y x0 y0 x1 y1 (inc x))
                ]
             (cond (<= x top-x (inc x))
                  ;; cuts top
                  (recur x (dec y)
                         (conj s [x (dec y)]))

                  ;; cuts right
                  (<= y right-y (inc y))
                  (recur (inc x) y
                         (conj s [(inc x) y]))

                  (and (= top-x (inc x))
                       (= right-y (dec y)))
                  (recur (inc x) (dec y)
                         (conj s
                               [x (dec y)]
                               [(inc x) y]
                               [(inc x) (dec y)]))

                  :default :up-right
                  ))))

      (and left? down?)
      (loop [x (int x0)
             y (int y0)
             s [[x y]]]
        (if (and (= x (Math/floor x1)) (= y (Math/floor y1)))
          s

          (let [bottom-x (intersect-x x0 y0 x1 y1 (inc y))
                left-y (intersect-y x0 y0 x1 y1 x)
                ]
            (cond (<= x bottom-x (inc x))
                  ;; cuts bottom
                  (recur x (inc y)
                         (conj s [x (inc y)]))

                  ;; cuts left
                  (<= y left-y (inc y))
                  (recur (dec x) y
                         (conj s [(dec x) y]))

                  (and (= bottom-x (dec x))
                       (= left-y (inc y)))
                  (recur (dec x) (inc y)
                         (conj s
                               [x (inc y)]
                               [(dec x) y]
                               [(dec x) (inc y)]))

                  :default :down-right))))

      (and left? up?)
      (loop [x (int x0)
             y (int y0)
             s [[x y]]]
        (if (and (= x (Math/floor x1)) (= y (Math/floor y1)))
          s

          (let [top-x (intersect-x x0 y0 x1 y1 y)
                left-y (intersect-y x0 y0 x1 y1 x)
                ]
            (cond (<= x top-x (inc x))
                  ;; cuts top
                  (recur x (dec y)
                         (conj s [x (dec y)]))

                  ;; cuts left
                  (<= y left-y (inc y))
                  (recur (dec x) y
                         (conj s [(dec x) y]))

                  (and (= top-x (dec x))
                       (= left-y (dec y)))
                  (recur (dec x) (dec y)
                         (conj s
                               [x (dec y)]
                               [(dec x) y]
                               [(dec x) (dec y)]))

                  :default :up-right
                  ))))



      right?
      (loop [x (int x0)
             y (int y0)
             s [[x y]]]
        (if (= x (Math/floor x1))
          s
          (let [right-y (intersect-y x0 y0 x1 y1 (inc x))]
            (assert (<= y right-y (inc y)) "intersection out of range")
            (recur (inc x) y
                   (conj s [(inc x) y])))))

      left?
      (loop [x (int x0)
             y (int y0)
             s [[x y]]]
        (if (= x (Math/floor x1))
          s
          (let [left-y (intersect-y x0 y0 x1 y1 x)]
            (assert (<= y left-y (inc y)) "intersection out of range")
            (recur (dec x) y
                   (conj s [(dec x) y])))))

      up?
      (loop [x (int x0)
             y (int y0)
             s [[x y]]]
        (if (= y (Math/floor y1))
          s
          (let [top-x (intersect-x x0 y0 x1 y1 y)]
            (assert (<= x top-x (inc x)) "intersection out of range")
            (recur x (dec y)
                   (conj s [x (dec y)])))))

      down?
      (loop [x (int x0)
             y (int y0)
             s [[x y]]]
        (if (= y (Math/floor y1))
          s
          (let [bottom-x (intersect-x x0 y0 x1 y1 (inc y))]
            (assert (<= x bottom-x (inc x)) "intersection out of range")
            (recur x (inc y)
                   (conj s [x (inc y)]))))))))
