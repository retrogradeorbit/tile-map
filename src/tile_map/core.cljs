(ns tile-map.core
  (:require [infinitelives.pixi.resources :as r]
            [infinitelives.pixi.texture :as t]
            [infinitelives.pixi.canvas :as c]
            [infinitelives.pixi.sprite :as s]
            [infinitelives.utils.events :as e]
            [infinitelives.utils.gamepad :as gp]
            [infinitelives.utils.vec2 :as vec2]
            [infinitelives.utils.console :refer [log]])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [infinitelives.pixi.macros :as m]))

(defonce bg-colour 0x202000)

(def tile-map
  [
"-BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB-"
"-                                                                   -"
"-                     OOOOOOOO                                      -"
"-            p                              c                 TTTTTT-"
"-          OOOOOO            OOOOO       TTTT          p   TTT-------"
"-OO                                  b   BBBB        TTTTOOBBBBBBBBB-"
"-     c                             TTTT          TTT----           -"
"-   OOOOOO     p                    ----         OBBB-BB-TT TTTOOOOO-"
"-            OOOOO   b   c    p     ----             -  --- ---     -"
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
  (#{"T" "-" "B" "O"} (get-tile-at x y)))

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

    (if (passable? nix niy)
      newpos

      ;; new tile collides
      (cond
        (and vert? (or up? down?)) (vec2/vec2 nx oy) ;; TODO: oy needs to be cast to edge
        (and horiz? (or left? right?)) (vec2/vec2 ox ny)
        (passable? nix oiy) (vec2/vec2 nx oy)
        (passable? oix niy) (vec2/vec2 ox ny)
        :default oldpos)
      )
    )
  )

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
                                        ;(s/set-texture! player (if (zero? (mod (int (/ fnum 10)) 2)) stand walk))

            (set-player player (int x) (int y) px py)

            (s/set-pos! batch (int x) (int y))
            (s/set-pos! background
                        (+ -2000 (mod (int (* x 0.90)) (* 4 32)))
                        (+ -2000 (mod (int (* y 0.90)) ( * 4 32))))

            (<! (e/next-frame))
            (recur pos
                   (inc fnum)
                   (-> joy
                       (vec2/scale 0.1)
                       (vec2/add ppos)
                       (constrain ppos)))))))))
