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
  (loop [x (Math/floor x0) y (Math/floor y0) s [[x y]]]
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

(defn cell-coverage-line-x [x0 y0 x1 y1 dx-fn]
  (loop [x (Math/floor x0)
         y (Math/floor y0)
         s [[x y]]]
    (if (= x (Math/floor x1))
      s
      (let [right-y (intersect-y x0 y0 x1 y1 (inc x))]
        (assert (<= y right-y (inc y)) "intersection out of range")
        (recur (dx-fn x) y
               (conj s [(dx-fn x) y]))))))

(defn cell-coverage-line-y [x0 y0 x1 y1 dy-fn]
  (loop [x (Math/floor x0)
         y (Math/floor y0)
         s [[x y]]]
    (if (= y (Math/floor y1))
      s
      (let [top-x (intersect-x x0 y0 x1 y1 (inc y))]
        (assert (<= x top-x (inc x)) "intersection out of range")
        (recur x (dy-fn y)
               (conj s [x (dy-fn y)]))))))

(defn all-covered [x0 y0 x1 y1]
  (match [(Math/sign (- x1 x0)) (Math/sign (- y1 y0))]

         ;; diagonals
         [1 1] (cell-coverage x0 y0 x1 y1 inc inc inc inc)
         [1 -1] (cell-coverage x0 y0 x1 y1 inc identity dec inc)
         [-1 1] (cell-coverage x0 y0 x1 y1 identity inc inc dec)
         [-1 -1] (cell-coverage x0 y0 x1 y1 identity identity dec dec)

         ;; compass directions
         [1 _] (cell-coverage-line-x x0 y0 x1 y1 inc)
         [-1 _] (cell-coverage-line-x x0 y0 x1 y1 dec)
         [_ -1] (cell-coverage-line-y x0 y0 x1 y1 dec)
         [_ 1] (cell-coverage-line-y x0 y0 x1 y1 inc)

         ;; new and old identical
         [0 0] [[(Math/floor x0) (Math/floor y0)]]
         ))

(defn vec2->parts [pos]
  (let [x (vec2/get-x pos)
        y (vec2/get-y pos)
        ix (int x)
        iy (int y)
        fx (- x ix)
        fy (- y iy)]
    [x y ix iy fx fy])
  )

(defn apply-edge-constraints [{:keys [passable?
                                      h-edge v-edge
                                      minus-h-edge minus-v-edge]}
                              oldpos newpos]
  (let [[nx ny nix niy nfx nfy] (vec2->parts newpos)
        [ox oy oix oiy ofx ofy] (vec2->parts oldpos)
        dx (- nix oix)
        dy (- niy oiy)

        <left-edge (< nfx h-edge)
        >right-edge (> nfx minus-h-edge)
        <top-edge (< nfy v-edge)
        >bottom-edge (> nfy minus-v-edge)

        pass? (passable? nix niy)
        pass-left? (passable? (dec nix) niy)
        pass-right? (passable? (inc nix) niy)
        pass-top? (passable? nix (dec niy))
        pass-bottom? (passable? nix (inc niy))
        pass-top-left? (passable? (dec nix) (dec niy))
        pass-bottom-left? (passable? (dec nix) (inc niy))
        pass-top-right? (passable? (inc nix) (dec niy))
        pass-bottom-right? (passable? (inc nix) (inc niy))]
    (match [<left-edge >right-edge <top-edge >bottom-edge
            pass-top-left? pass-top? pass-top-right?
            pass-left? pass? pass-right?
            pass-bottom-left? pass-bottom? pass-bottom-right?]

           ;; outer top-right corner
           [true _ _ true
            _ _ _
            true true _
            false true _]
           (let [ddx (- h-edge nfx) ddy (- nfy minus-v-edge)]
             (if (< ddx ddy)
               (vec2/vec2 (+ nix h-edge) ny)
               (vec2/vec2 nx (+ niy minus-v-edge))))

           ;; outer bottom-right corner
           [true _ true _
            false true _
            true true _
            _ _ _]
           (let [ddx (- h-edge nfx) ddy (- v-edge nfy)]
             (if (< ddx ddy)
               (vec2/vec2 (+ nix h-edge) ny)
               (vec2/vec2 nx (+ niy v-edge))))

           ;; outer top left
           [_ true _ true
            _ _ _
            _ true true
            _ true false]
           (let [ddx (- nfx minus-h-edge) ddy (- nfy minus-v-edge)]
             (if (< ddx ddy)
               (vec2/vec2 (+ nix minus-h-edge) ny)
               (vec2/vec2 nx (+ niy minus-v-edge))))

           ;; outer bottom left
           [_ true true _
            _ true false
            _ true true
            _ _ _]
           (let [ddx (- nfx minus-h-edge) ddy (- v-edge nfy)]
             (if (< ddx ddy)
               (vec2/vec2 (+ nix minus-h-edge) ny)
               (vec2/vec2 nx (+ niy v-edge))))

           ;; inner bottom left
           [true _ _ true
            _ _ _
            false true _
            _ false _]
           (vec2/vec2 (+ nix h-edge) (+ niy minus-v-edge))

           ;; inner top left
           [true _ true _
            _ false _
            false true _
            _ _ _]
           (vec2/vec2 (+ nix h-edge) (+ niy v-edge))

           ;; inner top right
           [_ true true _
            _ false _
            _ true false
            _ _ _]
           (vec2/vec2 (+ nix minus-h-edge) (+ niy v-edge))

           ;; inner bottom right
           [_ true _ true
            _ _ _
            _ true false
            _ false _]
           (vec2/vec2 (+ nix minus-h-edge) (+ niy minus-v-edge))

           ;; right edge
           [true _ _ _
            _ _ _
            false _ _
            _ _ _]
           (vec2/vec2 (+ nix h-edge) ny)

           ;; left edge
           [_ true _ _
            _ _ _
            _ _ false
            _ _ _]
           (vec2/vec2 (+ nix minus-h-edge) ny)

           ;; bottom edge
           [_ _ true _
            _ false _
            _ _ _
            _ _ _]
           (vec2/vec2 nx (+ niy v-edge))

           ;; top edge
           [_ _ _ true
            _ _ _
            _ _ _
            _ false _]
           (vec2/vec2 nx (+ niy minus-v-edge))

           ;; open space
           [_ _ _ _
            _ _ _
            _ _ _
            _ _ _]
           newpos)))

(defn intersect-diag
  "casting a line from [x0 y0] to [x1 y1], as it intersects the
  bounding square of cell [x y], where x-fn and y-fn identify which
  edges of [x y] to check, find the earliest intersection with the
  bounding square and return a vector specifying that intersection
  position."
  [x0 y0 x1 y1 x y x-fn y-fn]
  (let [top-x (intersect-x x0 y0 x1 y1 (y-fn y))
        left-y (intersect-y x0 y0 x1 y1 (x-fn x))]
    (cond (< x top-x (inc x))
          ;; cuts top
          (vec2/vec2 top-x (y-fn y))

          ;; cuts left
          (< y left-y (inc y))
          (vec2/vec2 (x-fn x) left-y))))

(defn intersect-compass-y
  "This is a faster form of intersect-diag that only works when the
  line is cast entirely vertical in cell space. ie, some fine lateral
  movement is allowed, but all the intersecting cells must be in a
  straight, vertical stack"
  [x0 y0 x1 y1 x y y-fn]
  (let [bottom-x (intersect-x x0 y0 x1 y1 (y-fn y))]
      (when (< x bottom-x (inc x))
        (vec2/vec2 bottom-x (y-fn y)))))

(defn intersect-compass-x
  "Same as intersect-compass-y but in a horizontal direction"
  [x0 y0 x1 y1 x y x-fn]
  (let [right-y (intersect-y x0 y0 x1 y1 (x-fn x))]
    (when (< y right-y (inc y))
      (vec2/vec2 (x-fn x) right-y))))

(defn intersect
  "Given a movement of a point from oldpos to newpos,
  calculate the exact position it intersects the outside
  of tile [x y]"
  [oldpos newpos x y]
  (let [[nx ny nix niy nfx nfy] (vec2->parts newpos)
        [ox oy oix oiy ofx ofy] (vec2->parts oldpos)]
    (match [(Math/sign (- nx ox)) (Math/sign (- ny oy))]
      [1 1] (intersect-diag ox oy nx ny x y identity identity)
      [-1 1] (intersect-diag ox oy nx ny x y inc identity)
      [1 -1] (intersect-diag ox oy nx ny x y identity inc)
      [-1 -1] (intersect-diag ox oy nx ny x y inc inc)
      [-1 _] (intersect-compass-x ox oy nx ny x y inc)
      [1 _] (intersect-compass-x ox oy nx ny x y identity)
      [_ -1] (intersect-compass-y ox oy nx ny x y inc)
      [_ 1] (intersect-compass-y ox oy nx ny x y identity)
      [0 0] (assert false "no movement!"))))

(defn reject
  "does the same as intersect, but instead of returning the exact
  point of intersection, returns a point slightly 'before' it (on the
  oldpos -> newpos line). Thus this point is garunteed to be in the
  neighboring tile."
  [oldpos newpos x y]
   (let [newpos (intersect oldpos newpos x y)]
     (-> (vec2/sub newpos oldpos)
        (vec2/scale 0.999)
        (vec2/add oldpos))))


(defn constrain [{:keys [passable?]
                  :as opts} newpos oldpos]
  (let [[nx ny nix niy nfx nfy] (vec2->parts newpos)
        [ox oy oix oiy ofx ofy] (vec2->parts oldpos)
        dx (- nix oix)
        dy (- niy oiy)]
    (if (and (> 2 (Math/abs dx))
             (> 2 (Math/abs dy))
             (> 2 (+ (Math/abs dx) (Math/abs dy))) ;; cant jump through diagonals
             (passable? nix niy))
      ;; small +/- 1 tile horiz/vert movements
      ;; in an open square. apply edge contsraints
      (apply-edge-constraints
       opts
       oldpos newpos)

      ;; new tile collides. moving so fast got embedded in other tile.
      ;; eject drastically!
      (let [points (all-covered ox oy nx ny)]
        (loop [[[x y] & r] points]
          (if (passable? x y)
            (if (zero? (count r))
              ;; no colision found
              newpos

              ;; try next point
              (recur r))

            ;; not passable! reject from this tile
            (apply-edge-constraints opts oldpos (reject oldpos newpos x y))))))))

(defn constrain-offset [opts offset newpos oldpos]
  (vec2/add
   offset
   (constrain opts (vec2/sub newpos offset) (vec2/sub oldpos offset))))
