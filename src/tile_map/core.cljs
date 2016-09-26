(ns tile-map.core
  (:require [infinitelives.pixi.resources :as r]
            [infinitelives.pixi.texture :as t]
            [infinitelives.pixi.canvas :as c]
            [infinitelives.pixi.sprite :as s]
            [infinitelives.utils.events :as e]
            [infinitelives.utils.gamepad :as gp]
            [infinitelives.utils.vec2 :as vec2]
            [infinitelives.utils.console :refer [log]]
            [cljs.core.match :refer-macros [match]]

            [tile-map.line :as line]
)
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [infinitelives.pixi.macros :as m]))

(defonce bg-colour 0x202000)

(def tile-map
  [
"-BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB-"
"-                                                                   -"
"-         c           OOOOOOOO                                      -"
"-       OOOO                                c                 TTTTTT-"
"-                            OOOOO       TTTT          p   TTT-------"
"-    c             OOOO              b   BBBB        TTTTOOBBBBBBBBB-"
"-OOOOOOOO                           TTTT          TTT----           -"
"-              OOOO                 ----         OBBB-BB-TT TTTOOOOO-"
"-                    b   c    p     ----             -  --- ---     -"
"-                  XXXX/XXXXXXXXXXXXBBBB        OOOOOB  BBB ---     -"
"-TTTTTTTTTTTTTTTTTTX   |                                    ---     -"
"-----.--o----------X   |                                TTTT---     -"
"-------------o--BBBX   | XXXXX                          BBBBBBB     -"
"-BBBBB----------bbb    |  0        b pb          b                  -"
"-     BB--------TTTXX  XXXXXXXX/XXXXXXXXXXX  XXXXXXXXX/X            -"
"-       BBB--------XXbb 0     p|             X        |             -"
"-          BBBBBBBBXXXXXXXXXXXX/XXXXXXXXXXXXXX        |             -"
"-                              |                      |             -"
"-                              |                      |             -"
"- p       X   X                |                      |             -"
"-TTTTTTT/TTTTTTTTTT           c|p            p        |             -"
"--------|----------TT c    OOOOOOOO     TTTTTTTTT/TTTTTT            -"
"-BBBBBBB|BBBBBBBBBBB-TT                 ---------|-o----            -"
"-       |           BB-TT p             --.------|------            -"
"-       |             B--TTTT  b        ---------|------            -"
"-       TTTTTT  p      BB----TTTTT      -------.-|------            -"
"-b      BBBB--TTT        BB-------O     ---------|------            -"
"-OO         --.--          BBBBBBB     T-----BBBB|BBBBBB           T-"
"-           ----.T                     ------    |                 --"
"- c    b    ------c    p      c        ------    X               TT--"
"-TTTTTTTTTTT------TTTTTTTTTTTTTTTTTTTTT------TTTTTTTTTTTTTTTTTTTT----"
"---------------------------------------------------------------------"
])

(defn make-tile-set [resource-key]
  (let [texture (r/get-texture resource-key :nearest)
        tile-lookup
        {
         "-" [0 0]
         "T" [16 0]
         "B" [0 16]
         "O" [16 16]
         "." [32 0]
         "o" [32 16]
         "X" [48 0]
         "0" [48 16]
         "/" [64 0]
         "|" [64 16]
         "b" [0 32]
         "p" [16 32]
         "c" [32 32]
         }
        ]
    (->> tile-lookup
         (map (fn [[c pos]] [c (t/sub-texture texture pos [16 16])]))
         (into {}))))

(defn get-tile-at [x y]
  (nth (tile-map y) x))

(defn not-passable? [x y]
  (#{"T" "-" "B" "O" "X"} (get-tile-at x y)))

(def passable? (comp not not-passable?))

(defn add-tiles! [batch tile-set tile-map]
  (doall
   (for [row (range (count tile-map))
         col (range (count (first tile-map)))]
     (let [char (nth (tile-map row) col)]
       (when (not= " " char)
         (.addChild batch
          (s/make-sprite (tile-set char)
                         :x (* 16 col) :y (* 16 row)
                         :xhandle 0 :yhandle 0)))))))

(defonce canvas
  (c/init {:layers [:bg :tilemap :ui]
           :background bg-colour
           :expand true}))

(defn set-player [player x y px py]
  (s/set-pos!
   player
   (+ x (* 16 4 px)) (+ y (* 16 4 py))))

(defn vec2->parts [pos]
  (let [x (vec2/get-x pos)
        y (vec2/get-y pos)
        ix (int x)
        iy (int y)
        fx (- x ix)
        fy (- y iy)]
    [x y ix iy fx fy])
  )

(def h-edge 0.3)
(def minus-h-edge (- 1 h-edge))
(def v-edge 0.40)
(def minus-v-edge (- 1 v-edge))

(defn apply-edge-constraints [oldpos newpos]
  (let [[nx ny nix niy nfx nfy] (vec2->parts newpos)
        [ox oy oix oiy ofx ofy] (vec2->parts oldpos)
        dx (- nix oix)
        dy (- niy oiy)

        down? (pos? dy)
        up? (neg? dy)
        horiz? (zero? dy)

        right? (pos? dx)
        left? (neg? dx)
        vert? (zero? dx)

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
        pass-bottom-right? (passable? (inc nix) (inc niy))

        not-pass-left? (not pass-left?)
        not-pass-right? (not pass-right?)
        not-pass-top? (not pass-top?)
        not-pass-bottom? (not pass-bottom?)
        not-pass-top-left? (not pass-top-left?)
        not-pass-bottom-left? (not pass-bottom-left?)
        not-pass-top-right? (not pass-top-right?)
        not-pass-bottom-right? (not pass-bottom-right?)]
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
            false false _]
           (vec2/vec2 (+ nix h-edge) (+ niy minus-v-edge))

           ;; inner top left
           [true _ true _
            false false _
            false true _
            _ _ _]
           (vec2/vec2 (+ nix h-edge) (+ niy v-edge))

           ;; inner top right
           [_ true true _
            _ false false
            _ true false
            _ _ _]
           (vec2/vec2 (+ nix minus-h-edge) (+ niy v-edge))

           ;; inner bottom right
           [_ true _ true
            _ _ _
            _ true false
            _ false false]
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
           newpos))
  )

(defn intersect [oldpos newpos x y]
  (let [[nx ny nix niy nfx nfy] (vec2->parts newpos)
        [ox oy oix oiy ofx ofy] (vec2->parts oldpos)

        dx (- nx ox)
        dy (- ny oy)

        down? (pos? dy)
        up? (neg? dy)
        horiz? (zero? dy)

        right? (pos? dx)
        left? (neg? dx)
        vert? (zero? dx)
        ]
    (log left? right? up? down?)
    (cond
      (and right? down?)
      (let [top-x (line/intersect-x ox oy nx ny y)
            left-y (line/intersect-y ox oy nx ny x)]
        (log top-x left-y)
        (cond (< x top-x (inc x))
          ;; cuts top
          (vec2/vec2 top-x y)

          ;; cuts left
          (< y left-y (inc y))
          (vec2/vec2 x left-y)))

      (and left? down?)
      (let [top-x (line/intersect-x ox oy nx ny y)
            right-y (line/intersect-y ox oy nx ny (inc x))]
        (cond (< x top-x (inc x))
          ;; cuts top
          (vec2/vec2 top-x y)

          ;; cuts right
          (< y right-y (inc y))
          (vec2/vec2 (inc x) right-y)))

      (and right? up?)
      (let [bottom-x (line/intersect-x ox oy nx ny (inc y))
            left-y (line/intersect-y ox oy nx ny x)]
        (log bottom-x left-y)
        (cond (< x bottom-x (inc x))
          ;; cuts bottom
          (vec2/vec2 bottom-x (inc y))

          ;; cuts left
          (< y left-y (inc y))
          (vec2/vec2 x left-y)))

      (and left? up?)
      (let [bottom-x (line/intersect-x ox oy nx ny (inc y))
            right-y (line/intersect-y ox oy nx ny (inc x))]
        (cond (< x bottom-x (inc x))
          ;; cuts bottom
          (vec2/vec2 bottom-x (inc y))

          ;; cuts right
          (< y right-y (inc y))
          (vec2/vec2 (inc x) right-y)))

      left?
      (let [right-y (line/intersect-y ox oy nx ny (inc x))]
        (when (< y right-y (inc y))
          (vec2/vec2 (inc x) right-y)))

      right?
      (let [left-y (line/intersect-y ox oy nx ny x)]
        (when (< y left-y (inc y))
          (vec2/vec2 x left-y)))

      up?
      (let [bottom-x (line/intersect-x ox oy nx ny (inc y))]
        (when (< x bottom-x (inc x))
          (vec2/vec2 bottom-x (inc y))))

      down?
      (let [top-x (line/intersect-x ox oy nx ny y)]
        (when (< x top-x (inc x))
          (vec2/vec2 top-x y)))

      :default :foo
      )


    )
  )

(defn reject [oldpos newpos x y]
  (log "op" oldpos "np" newpos "x" x "y" y)
  (let [newpos (intersect oldpos newpos x y)]
    (log "NP" newpos)
    (-> (vec2/sub newpos oldpos)
        (vec2/scale 0.999)
        (vec2/add oldpos))))


(log
 #_ (reject (vec2/vec2 1.9 0.5) (vec2/vec2 2.5 2.3) 1 0)
 #_ (intersect (vec2/vec2 20.22 5.4) (vec2/vec2 30.49 4.86) 30 4)
 ;(intersect (vec2/vec2 20.68 9) (vec2/vec2 20.93 9) 20 9)
 (str (line/all-covered 6 9 6 11))
)


(defn constrain [newpos oldpos]
  (let [[nx ny nix niy nfx nfy] (vec2->parts newpos)
        [ox oy oix oiy ofx ofy] (vec2->parts oldpos)
        dx (- nix oix)
        dy (- niy oiy)

        down? (pos? dy)
        up? (neg? dy)
        horiz? (zero? dy)

        right? (pos? dx)
        left? (neg? dx)
        vert? (zero? dx)
        ]
    (when (gp/button-pressed? 0 :x)
      (log oix oiy))

    (if (and (> 2 (Math/abs dx)) (> 2 (Math/abs dy)))
      ;; small +/- 1 tile movements
      (if (passable? nix niy)
        ;; in an open square. apply edge contsraints
        (do (log "open passable")
            (apply-edge-constraints oldpos newpos))

        ;; new tile collides. moving so fast got embedded in other tile. eject drastically!
        (do (log "small movement not passable" ox oy nx ny )
            (let [points (line/all-covered ox oy nx ny)]
              (log "small" dx dy  oix oiy nix niy (str points))
              (loop [[[x y] & r] points]
                (log [x y] (passable? x y))
                (if (passable? x y)
                  (if (zero? (count r))
                    ;; no colision found
                    newpos

                    ;; try next point
                    (recur r))

                  ;; not passable! reject from this tile
                  (let [np (reject oldpos newpos x y)]
                    (log "np---->" np)
                    (apply-edge-constraints oldpos np)))))

            #_            (let [np (reject oldpos newpos nix niy)]
                            (log "np is" (str np))
                            (apply-edge-constraints
                             oldpos np



                             ;; (cond
                             ;;   (and vert? (or up? down?)) (vec2/vec2 nx (+ oiy (if up? v-edge minus-v-edge)))
                             ;;   (and horiz? (or left? right?)) (vec2/vec2 (+ oix (if left? h-edge minus-h-edge)) ny)
                             ;;   (passable? nix oiy) (vec2/vec2 nx (+ oiy (if up? v-edge minus-v-edge)))
                             ;;   (passable? oix niy) (vec2/vec2 (+ oix (if left? h-edge minus-h-edge)) ny)
                             ;;   :default oldpos)
                             ))))

      ;; large >=2 tile movements
      ;; walk from start to finish tile. check each one for passability
      ;; when you find the first unpassable one, eject drastically from this tile
      (let [points (line/all-covered oix oiy nix niy)]
        (log "LARGE JUMP" dx dy  oix oiy nix niy (str points))
        (loop [[[x y] & r] points]
          (log [x y] (passable? x y))
          (if (passable? x y)
            (if (zero? (count r))
              ;; no colision found
              newpos

              ;; try next point
              (recur r))

            ;; not passable! reject from this tile
            (let [np (reject oldpos newpos x y)]
              (log "np---->" np)
              (apply-edge-constraints oldpos np))))))))

(defonce main
  (go
    ;; load image tilesets
    (<! (r/load-resources canvas :ui ["img/tiles.png"]))

    ;; make the tile texture lookup
    (let [tile-set (make-tile-set :tiles)
          bg (js/PIXI.TilingSprite.
              (t/sub-texture
               (r/get-texture :tiles :nearest)
               [0 48] [32 32])
              1000 1000)
          batch (js/PIXI.ParticleContainer.)
          stand (t/sub-texture (r/get-texture :tiles :nearest) [0 96] [16 16])
          walk (t/sub-texture (r/get-texture :tiles :nearest) [16 96] [16 16])
          ]
      (add-tiles! batch tile-set tile-map)
      (s/set-scale! batch 4)
      (s/set-scale! bg 4)

      ;; nearest
      (m/with-sprite canvas :tilemap
        [background bg
         tilemap batch
         player (s/make-sprite stand :scale 4)]
        (loop [pos (vec2/vec2 -500 -400) fnum 0
               ppos (vec2/vec2 1.5 4.5)]
          (let [x (vec2/get-x pos) ;; (+ -2000 (* 1000 (Math/sin theta)))
                y (vec2/get-y pos) ;; (+ -1000 (* 500 (Math/cos theta)))

                px (vec2/get-x ppos)
                py (vec2/get-y ppos)
                pix (int px)
                piy (int py)
                dx (- px pix)
                dy (- py piy)

                joy (vec2/vec2 (or (gp/axis 0) 0)
                               (or (gp/axis 1) 0))
                ]

            (s/set-texture! player (if (zero? (mod (int (/ fnum 10)) 2)) stand walk))

            (set-player player (int x) (int y) px py)

            (s/set-pos! batch (int x) (int y))
            (s/set-pos! background
                        (+ -2000 (mod (int (* x 0.90)) (* 4 32)))
                        (+ -2000 (mod (int (* y 0.90)) ( * 4 32))))

            (<! (e/next-frame))
            (recur (-> ppos
                       (vec2/scale (* -2 32)))
                   (inc fnum)
                   (-> joy
                       (vec2/scale 1.5)
                       (vec2/add ppos)
                       (constrain ppos)))))))))
