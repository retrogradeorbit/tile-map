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
"-         c        OOOOOOOOOOO                                      -"
"-       OOOO      T           T             c                 TTTTTT-"
"-                 B           -OOO       TTTT          p   TTT-------"
"-    c             OOOO       B      b   BBBB      TTTTTTOOBBBBBBBBB-"
"-OOOOOOOO              OOOOOOO      TTTT          T-                -"
"-              OOOO                 ---B         OBB -BB-TT TTTOOOOO-"
"-                    b   c    p     ---              -  --- ---     -"
"-                  XXXX/XXXXXXXXXXXXBBB         OOOOOB  BBB ---     -"
"-TTTTTTTTTTTTTTTTTTX   |                                    ---     -"
"-----.--o----------X   |                                TTTT---     -"
"-------------o--BBBX   | XXXXX                          BBBBBBB     -"
"-BBBBB----------bbb    |  0                      b                  -"
"-     BB--------TTTXX  XXXXXXXX/XX            XXXXXXXX/X            -"
"-       BBB--------XXbb 0      |                      |             -"
"-          BBBBBBBBXXXXXXX     |                      |             -"
"-                              |                      |             -"
"-                     XXXXXXXX |       /              |             -"
"- p       X   X                |       |              |             -"
"-TTTTTTT/TTTTTTTTTT           c|p      |     p        |             -"
"--------|----------TT c    OOOOOOOO    |TTTTTTTTTTTTTTTT            -"
"-BBBBBBB|BBBBBBBBBBB-TT                |--BBBB-----o----            -"
"-       |           BB-TT p            |--    ----------            -"
"-       |             B--TTTT  b       |--    BBBB------            -"
"-       TTTTTT  p      BB----TTTTT     |-B       |------            -"
"-b      BBBB--TTT        BB-------T    TB   TTTTT|------            -"
"-OO         --.--          BBBBBBB-TTTTB   T-BBBB|BBBBBB           T-"
"-           ----.T                BBBBB   T--    |                 --"
"- c    b    ------c    p      c          T---    X               TT--"
"-TTTTTTTTTTT------TTTTTTTTTTTTTTTTTTTTTTT----TTTTTTTTTTTTTTTTTTTT----"
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

(defn walkable? [x y]
  (if (= "/" (get-tile-at x y))
    false
    (passable? x y))
  )

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

(def h-edge 0.3)
(def minus-h-edge (- 1 h-edge))
(def v-edge 0.40)
(def minus-v-edge (- 1 v-edge))

(defn hollow
  "if vector is less than a certain size, make it zero"
  [v lim]
  (if (< (vec2/magnitude-squared v) lim)
    (vec2/zero)
    v))

(def jump-accel-1 0.1)
(def jump-accel-2+ 0.03)
(def jump-frames 10)

(defn jump-button-pressed? []
  (or
   (e/is-pressed? :z)
   (e/is-pressed? :space)
   (gp/button-pressed? 0 :x)))

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

          gravity (vec2/vec2 0 0.01)
          ]
      (add-tiles! batch tile-set tile-map)
      (s/set-scale! batch 4)
      (s/set-scale! bg 4)

      ;; nearest
      (m/with-sprite canvas :tilemap
        [background bg
         tilemap batch
         player (s/make-sprite stand :scale 4)]
        (loop [
               state :walking
               fnum 0
               old-vel (vec2/vec2 0 0)
               ppos (vec2/vec2 1.5 4.5)
               jump-pressed 0]
          (let [
                old-pos ppos
                pos (-> ppos
                        (vec2/scale (* -2 32)))
                x (vec2/get-x pos) ;; (+ -2000 (* 1000 (Math/sin theta)))
                y (vec2/get-y pos) ;; (+ -1000 (* 500 (Math/cos theta)))

                px (vec2/get-x ppos)
                py (vec2/get-y ppos)
                pix (int px)
                piy (int py)
                dx (- px pix)
                dy (- py piy)

                joy (vec2/vec2 (or (gp/axis 0)
                                   (cond (e/is-pressed? :left) -1
                                         (e/is-pressed? :right) 1
                                         :default 0) )
                               (or (gp/axis 1)
                                   (cond (e/is-pressed? :up) -1
                                         (e/is-pressed? :down) 1
                                         :default 0)
                                   ))
                ]

            (s/set-texture! player (if (zero? (mod (int (/ fnum 10)) 2)) stand walk))
            (set-player player (int x) (int y) px py)
            (s/set-pos! batch (int x) (int y))
            (s/set-pos! background
                        (+ -2000 (mod (int (* x 0.90)) (* 4 32)))
                        (+ -2000 (mod (int (* y 0.90)) ( * 4 32))))

            (<! (e/next-frame))
                                        ;(log dy minus-v-edge)
            (let [
                  square-on (get-tile-at pix piy)
                  square-below (get-tile-at pix (inc piy))

                  square-standing-on (get-tile-at pix
                                                  (int (+ 0.3 py)))

                  on-ladder-transition? (and
                                         (or (= square-on "|")
                                             (= square-on "/")
                                        ;(= square-below "|")
                                             (= square-below "/")
                                             ))



                  on-ladder? (or (= square-standing-on "|")
                                 (= square-standing-on "/"))

                  ladder-up? (or (= square-standing-on "|")
                                 (= square-standing-on "/"))

                  ladder-down? (or (= square-below "|")
                                   (= square-below "/"))

                  ;; simulate a little vertical move down to see if we are
                  ;; standing on solid ground
                  fallen-pos (line/constrain {:passable? walkable?
                                              :h-edge h-edge
                                              :v-edge v-edge
                                              :minus-h-edge minus-h-edge
                                              :minus-v-edge minus-v-edge}
                                             (vec2/add old-pos (vec2/vec2 0 0.1)) old-pos)

                  standing-on-ground? (= (vec2/get-y fallen-pos) (vec2/get-y old-pos))

                  jump-pressed (cond
                                 (and (jump-button-pressed?) (zero? jump-pressed) standing-on-ground?) ;; cant jump off ladder! if you can, problem... when jumping off lader, state stays climbing causing no accel for the jump
                                 (inc jump-pressed)

                                 (and (jump-button-pressed?) (pos? jump-pressed))
                                 (inc jump-pressed)

                                 :default
                                 0)

                                        ;_ (log "jump pressed" jump-pressed)

                  jump-force (if (and (<= 1 jump-pressed jump-frames)
                                      (jump-button-pressed?))
                               (vec2/vec2 0 (- (if (= 1 jump-pressed)
                                                 jump-accel-1
                                                 jump-accel-2+)))
                               (vec2/zero))

                  joy-dy (-> joy
                             (hollow 0.2)
                             (vec2/get-y))

                  joy-dy (cond
                           (and (not ladder-up?) (neg? joy-dy))
                           0

                           (and (not ladder-down?) (not ladder-up?) (pos? joy-dy))
                           0

                           :default joy-dy)

                  joy-dx (-> joy
                             (hollow 0.2)
                             (vec2/scale 0.01)
                             (vec2/get-x))

                  state-ladder? (and on-ladder-transition?
                                     (not (zero? joy-dy)))

                  joy-acc (if (= :climbing state)
                            (vec2/vec2 joy-dx (* 0.1 joy-dy))
                            (vec2/vec2 joy-dx 0))


                  #_ (log (str joy-acc))

                  player-vel-x (Math/abs (vec2/get-x old-vel))

                  ;; when moving quickly left and right, and the
                  ;; joystick is centered or reversed, this breaking
                  ;; horitontal force is applied
                  player-brake
                  (match [(Math/sign (vec2/get-x old-vel))
                          (Math/sign (vec2/get-x (hollow joy 0.5)))]
                         [1 0] (vec2/vec2 -1 0)
                         [1 -1] (vec2/vec2 -1 0)
                         [-1 0] (vec2/vec2 1 0)
                         [-1 1] (vec2/vec2 1 0)
                         [_ _] (vec2/zero))


                  next-state (if state-ladder?
                               :climbing
                               (if on-ladder? state :walking))

                  passable-fn (if (= :walking next-state)
                                             walkable?
                                             passable?)

                  new-vel (-> old-vel
                              (vec2/set-y (if (= state :climbing) 0 (vec2/get-y old-vel)))
                              (vec2/add (if (= state :climbing) (vec2/zero) gravity))
                              #_ (vec2/add gravity)
                              (vec2/add jump-force)
                              (vec2/add joy-acc)
                              (vec2/add (vec2/scale player-brake (/ player-vel-x 3)))
                              (vec2/scale 0.98))

                  new-pos (-> old-pos
                              (vec2/add new-vel))

                  con-pos (line/constrain {:passable? passable-fn
                                           :h-edge h-edge
                                           :v-edge v-edge
                                           :minus-h-edge minus-h-edge
                                           :minus-v-edge minus-v-edge}
                                          new-pos old-pos)


                  #_ (log dy on-ladder? ladder-up? ladder-down? state state-ladder? next-state (str joy-acc) (str player-brake) ;(str new-vel) (vec2/get-y old-vel) (str old-pos "=>" con-pos)
                       )



                  old-vel (if (= :walking state) (vec2/sub con-pos old-pos)
                              (-> (vec2/sub con-pos old-pos)
                                  (vec2/set-y 0)))

                  ;_ (log (str "!" old-vel))
                  ]
              (if (< (vec2/get-x old-vel) 0)
                (s/set-scale! player -4 4)
                (s/set-scale! player 4 4)
                )
              (recur
               next-state
               (inc fnum)
               old-vel
               con-pos
               jump-pressed
               ))))))))
