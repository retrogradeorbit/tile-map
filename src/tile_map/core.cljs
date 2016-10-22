(ns tile-map.core
  (:require [infinitelives.pixi.resources :as r]
            [infinitelives.pixi.texture :as t]
            [infinitelives.pixi.canvas :as c]
            [infinitelives.pixi.sprite :as s]
            [infinitelives.pixi.pixelfont :as pf]
            [infinitelives.utils.events :as e]
            [infinitelives.utils.gamepad :as gp]
            [infinitelives.utils.vec2 :as vec2]
            [infinitelives.utils.console :refer [log]]
            [cljs.core.match :refer-macros [match]]
            [clojure.string :as string]
            [tile-map.line :as line]
            [tile-map.map :as tm]
            [cljs.core.async :refer [chan close! >! <! timeout]]
)
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [infinitelives.pixi.macros :as m]
                   [infinitelives.pixi.pixelfont :as pf]
))

(defonce bg-colour 0x202000)

(def tile-map
  (-> [
"------------------------XXXXX------------------------------------------------------"
"-                         b X                                     b   -------------"
"-         c     ----XXXXXXXXX                               X/XXXXXX  ---      ----"
"-       ----        X                       c                |        --        ---"
"-                   X                    ----          p     |p  c    --        ---"
"-    c              X                b   ----      ---------------------      -----"
"---------           X               ----          --                       --------"
"-                   X               ----         --- ------ -----------------------"
"-                   Xb   c    p     ---              -  --- -----------------------"
"-                   XXX/XXXXXXXXXXXX---         ------  --- -----------------   |--"
"-            -      X  |                                    ------------bbbbbbb |--"
"-           --      X  |                                ----------------------- |--"
"------------------- X  | XXXXX                          -------     ----------- |--"
"--------.o----  bbb    |  0                      b                  ----------- |--"
"-     -------------XX  XXXXX X X X            XXXXXXXX/X            ----        |--"
"-       -----------Xpbb 0                             |   b b       ---- bbbb------"
"----/      ---XXXXXXXXXXXX                          XXXXXXXXXXXX    ---- ----------"
"-   |         Xbb     Xbb              /              |             ---- ----------"
"-   |         XXXXXXX XXXXXXX X XX     |              |             ----        ---"
"- p |     X   X   c      Xc      X     |              |             ------- bbbb---"
"--------/-----X XXXXXXX XXXXXX XXX     |     p        |             ------- bbbb---"
"--------|--.o-  X    c    X      X     |----------------                    bbbb---"
"--------|-----XXX XXXXXXXXXXXXXX X     |-----------o----            ---------------"
"-       |       X   X   X    c   X     |--    ----------            ---------------"
"-       |       XXX X X bX XXXXXXX     |--    ----------            ---------------"
"-       ------ bX      XX  X     X     |--       |------            ---------------"
"-b      --------X XXX XXX XX XXX X-    --   -----|------            ---------------"
"---         --.-X  bX  c     Xb  X------   ------|------           ----------------"
"-           ----XX XXXXXXXXXXXXXXX-----   ---    |                 ----------------"
"- c    b               p      c          ----    X               ------------------"
"-----------------------------------------------------------------------------------"
"-----------------------------------------------------------------------------------"
       ]
      tm/strs->keymap tm/remap-keymap))

(def platform-map
  (->
   [
    "    "
    " ---"
    "  - "]
   tm/strs->keymap tm/remap-keymap
   ))

(def platform2-map
  (->
   [
    "    "
    " XXX"]
   tm/strs->keymap tm/remap-keymap))

(def sprite-sheet-layout
  {
   :dynamite-1 {:size [16 16] :pos [112 32]}
   :dynamite-2 {:size [16 16] :pos [96 32]}
   :dynamite-3 {:size [16 16] :pos [80 32]}
   :dynamite-4 {:size [16 16] :pos [64 32]}
   :dynamite-5 {:size [16 16] :pos [48 32]}

   :explosion-1 {:size [16 16] :pos [48 48]}
   :explosion-2 {:size [16 16] :pos [64 48]}
   :explosion-3 {:size [16 16] :pos [80 48]}
   :explosion-4 {:size [16 16] :pos [96 48]}
   :explosion-5 {:size [16 16] :pos [112 48]}
   :explosion-6 {:size [16 16] :pos [128 48]}

   :gold {:size [16 16] :pos [0 32]}
   })

(defn make-foreground-map [bg-map-lines]
  (tm/mapv-mapv
   bg-map-lines
   (fn [c]
     (cond
       (= c :web) :web
       (= c :ladder-top) :ladder-top-fg
       :default :space))))

(defn not-passable? [x y]
  (tm/not-passable? (tm/get-tile-at tile-map x y)))

(def passable? (comp not not-passable?))

(defn not-platform-passable? [x y]
  (tm/not-passable? (tm/get-tile-at platform-map x y)))

(def platform-passable? (comp not not-platform-passable?))

(defn not-platform2-passable? [x y]
  (tm/not-passable? (tm/get-tile-at platform2-map x y)))

(def platform2-passable? (comp not not-platform2-passable?))

(defn walkable? [x y]
  (tm/walkable? (tm/get-tile-at tile-map x y)))

(defonce canvas
  (c/init {:layers [:bg :tilemap :stats :title :ui]
           :background bg-colour
           :expand true
           :origins {:stats :bottom-left
                     :title :top}
           :translate {:stats [40 -40]
                       :title [0 40]}}))

(s/set-default-scale! 1)

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

(defn platform-constrain [pass? pos old-pos new-pos]
  (line/constrain-offset
   {:passable? pass?
    :h-edge h-edge
    :v-edge v-edge
    :minus-h-edge minus-h-edge
    :minus-v-edge minus-v-edge}
   pos new-pos old-pos))

(defn dynamite-constrain [pass? pos old-pos new-pos]
  (line/constrain-offset
   {:passable? pass?
    :h-edge 0.1
    :v-edge 0.3
    :minus-h-edge 0.9
    :minus-v-edge 0.5}
   pos new-pos old-pos))

;; work out if we are standing on a platform, and if
;; so, which one?  to do this, we simulate the
;; player falling every so slightly by a small
;; amount and by a larger amount. we then constrain
;; both these movements by the platform in
;; question. If the y value is the same between the
;; two end points, then we are standing on the
;; platform (even if platform is moving!)
(defn which-platform? [old-pos platforms]
  (let [start old-pos
        end1 (vec2/add old-pos (vec2/vec2 0 0.1))
        end2 (vec2/add old-pos (vec2/vec2 0 0.3))]
    (loop [n 0]
      (let [{:keys [passable? platform-pos]} (nth platforms n)]
        (if (= (vec2/get-y (platform-constrain passable? platform-pos start end1))
               (vec2/get-y (platform-constrain passable? platform-pos start end2)))
          n
          (when (< n 3)
            (recur (inc n))))))))

(defn make-text-display
  "create an updating number display with icon. Used for gold/dynamite etc.
  icon and text appeads at `y` with font `font` displaying string `s`.
  returns a channel. When you push a value down the channel, the
  display will update to that pushed value. When you close! the channel,
  the text disappears.
  "
  [icon y font s]
  (let [c (chan)]
    (go
      (m/with-sprite :stats
        [icon (s/make-sprite icon :scale 4 :y (+ y -5))
         text (pf/make-text font (str s) :scale 4 :xhandle 0 :x 50 :y y)]
        (loop [s (<! c)]
          (pf/change-text! text font (str s))
          (s/update-handle! text 0 0.5)
          (when-let [res (<! c)]
            (recur res)))))
    c))

(def platforms
  [{:name :level
    :fn (fn [_] (vec2/zero))
    :passable? walkable?
    :apply? (fn [_] true)}

   {:name :t-platform
    :fn (fn [fnum] (vec2/vec2 9 (+ 7 (* 2.01 (Math/sin (/ fnum 60))))))
    :passable? platform-passable?
    :apply? (fn [pos] true)}

   {:name :diagonal
    :fn (fn [fnum] (vec2/vec2 (+ 56 (* 3 (Math/sin (/ fnum 40))))
                              (+ 23 (* 3 (Math/sin (/ fnum 40))))))
    :passable? platform2-passable?
    :apply? (fn [pos] true)}

   {:name :horizontal
    :fn (fn [fnum] (vec2/vec2 (+ 62 (* 3 (Math/sin (/ fnum 60)))) 20))
    :passable? platform2-passable?
    :apply? (fn [pos] true)}])

(def platform-fn (:fn (platforms 1)))
(def platform2-fn (:fn (platforms 2)))
(def platform3-fn (:fn (platforms 3)))

(defn constrain-pos [constrain-fn platforms old-pos new-pos]
  (reduce
   (fn [pos {:keys [passable? platform-pos]}]
     (constrain-fn passable? platform-pos old-pos pos))
   new-pos platforms))

(defn prepare-platforms [platforms fnum]
  (->> platforms
       (map #(let [platform-pos ((:fn %) fnum)
                   old-platform-pos ((:fn %) (dec fnum))]
               (assoc %
                      :platform-pos platform-pos
                      :old-platform-pos old-platform-pos
                      :platform-delta (vec2/sub platform-pos old-platform-pos))))))

(defn filter-platforms [platforms pos]
  (->> platforms
       (filter #((:apply? %) pos))))

(def gravity (vec2/vec2 0 0.01))

(defn make-dynamite [container pos vel start-frame]
  (go
    (m/with-sprite container
      [sprite (s/make-sprite :dynamite-5 :scale 1 :x (vec2/get-x pos) :y (vec2/get-y pos))]
      (loop [n 0
             p pos
             v vel]
        (s/set-pos! sprite (vec2/scale p 16))
        (<! (e/next-frame))
        (when (< n 300)
          (if (pos? (mod n 3))
            (let [frame (int (/ n 60))
                  texture (nth [:dynamite-5 :dynamite-4 :dynamite-3 :dynamite-2 :dynamite-1] frame)]
              (s/set-texture! sprite (t/get-texture texture)))
            (s/set-texture! sprite (t/get-texture :dynamite-1)))
          (let [platform-state (prepare-platforms platforms n (vec2/zero))
                new-pos (constrain-pos dynamite-constrain
                                       (prepare-platforms platforms (+ 1 start-frame n) p)
                                       p (vec2/add p v))
                new-vel (-> new-pos
                            (vec2/sub p)
                            (vec2/add gravity)
                            (vec2/scale 0.98))]
            (recur (inc n)
                   new-pos
                   new-vel
                   ))))

      ;; explode
      (loop [[f & r] [:explosion-1 :explosion-2
                      :explosion-3 :explosion-4
                      :explosion-5 :explosion-6]]
        (s/set-texture! sprite (t/get-texture f))
        (<! (e/next-frame))
        (<! (e/next-frame))
        (when r
          (recur r))
        )
      )))

(defonce main
  (go
    ;; load image tilesets
    (<! (r/load-resources canvas :ui ["img/tiles.png"]))

    ;; load textures
    (t/load-sprite-sheet! (r/get-texture :tiles) sprite-sheet-layout)

    ;; make a number font
    (pf/pixel-font :numbers "img/tiles.png" [0 128] [79 159]
                   :chars "0123456789"
                   :space 5)

    ;; make a small pixelly font
    (pf/pixel-font :pixel "img/tiles.png" [48 64] [120 83]
                   :chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ.!?0123456789+-$'\""
                   :space 3)

    (go
      (m/with-sprite :title
        [title (pf/make-text :pixel "DYNA-MINER 0.3"
                             :scale 4 :xhandle 0.5)

         ]
        (loop []
          (<! (e/next-frame))
          (recur))))

    ;; make the tile texture lookup
    (let [tile-set (tm/make-tile-set :tiles)
          stand (t/sub-texture (r/get-texture :tiles :nearest) [0 96] [16 16])
          walk (t/sub-texture (r/get-texture :tiles :nearest) [16 96] [16 16])
          tilemap-order-lookup (tm/make-tiles-struct tile-set tile-map)

          dynamite (make-text-display :dynamite-5 0 :numbers 10)
          gold (make-text-display :gold -64 :numbers 0)
          ]

      ;; create sprite and tile map batches
      (m/with-sprite canvas :tilemap
        [background (js/PIXI.TilingSprite.
                     (t/sub-texture
                      (r/get-texture :tiles :nearest)
                      [0 48] [32 32])
                     1000 1000)
         tilemap (s/make-container
                  :children (tm/make-tiles tile-set tile-map)
                  :xhandle 0 :yhandle 0
                  :scale 4
                  :particle true)
         platform (s/make-container
                   :children (tm/make-tiles tile-set platform-map)
                   :xhandle 0 :yhandle 0
                   :scale 4
                   :particle true)
         platform2 (s/make-container
                    :children (tm/make-tiles tile-set platform2-map)
                    :xhandle 0 :yhandle 0
                    :scale 4
                    :particle true)
         platform3 (s/make-container
                    :children (tm/make-tiles tile-set platform2-map)
                    :xhandle 0 :yhandle 0
                    :scale 4
                    :particle true)
         player (s/make-sprite stand :scale 4)
         foreground (s/make-container
                     :children (tm/make-tiles tile-set (into [] (make-foreground-map tile-map)))
                     :xhandle 0 :yhandle 0
                     :scale 4
                     :particle true)
         dynamites (s/make-container :scale 4 :particle false)

         ]
        (s/set-scale! background 4)
        (loop [
               state :walking
               fnum 0
               old-vel (vec2/vec2 0 0)
               ppos (vec2/vec2 1.5 4.5)
               jump-pressed 0
               gold-num 0
               dynamite-num 10
               last-x-pressed? (e/is-pressed? :x)
               facing :left
               ]

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

                platform-pos (platform-fn fnum)
                old-platform-pos (platform-fn (dec fnum))
                platform-delta (vec2/sub platform-pos old-platform-pos)

                platform2-pos (platform2-fn fnum)
                old-platform2-pos (platform2-fn (dec fnum))
                platform2-delta (vec2/sub platform2-pos old-platform2-pos)

                platform3-pos (platform3-fn fnum)
                old-platform3-pos (platform3-fn (dec fnum))
                platform3-delta (vec2/sub platform3-pos old-platform3-pos)
                ]

            (s/set-texture! player
                            (if (> (Math/abs (vec2/get-x joy)) 0.02)
                              (if (zero? (mod (int (/ fnum 10)) 2)) stand walk)
                              stand))
            (set-player player (int x) (int y) px py)
            (s/set-pos! tilemap (int x) (int y))
            (s/set-pos! dynamites (int x) (int y))
            (s/set-pos! platform (vec2/add
                                  (vec2/scale old-platform-pos (* 4 16))
                                  (vec2/vec2 x y)))
            (s/set-pos! platform2 (vec2/add
                                   (vec2/scale old-platform2-pos (* 4 16))
                                   (vec2/vec2 x y)))
            (s/set-pos! platform3 (vec2/add
                                   (vec2/scale old-platform3-pos (* 4 16))
                                   (vec2/vec2 x y)))
            (s/set-pos! foreground (int x) (int y))
            (s/set-pos! background
                        (+ -2000 (mod (int (* x 0.90)) (* 4 32)))
                        (+ -2000 (mod (int (* y 0.90)) ( * 4 32))))

            (<! (e/next-frame))
                                        ;(log dy minus-v-edge)
            (let [
                  square-on (tm/get-tile-at tile-map pix piy)
                  square-below (tm/get-tile-at tile-map pix (inc piy))
                  square-standing-on (tm/get-tile-at tile-map pix
                                                     (int (+ 0.3 py)))

                  on-ladder-transition? (or (= square-on :ladder)
                                            (= square-on :ladder-top)
                                            (= square-below :ladder-top))

                  on-ladder? (#{:ladder :ladder-top} square-standing-on)

                  on-gold? (= :gold square-standing-on)
                  new-gold (or
                            (when on-gold?
                              (when (tilemap-order-lookup [pix piy])
                                (let [child
                                      (.getChildAt tilemap
                                                   (tilemap-order-lookup [pix piy]))]
                                  (when (= 1 (.-alpha child))
                                    (s/set-alpha! child 0)
                                    (>! gold (inc gold-num))
                                    (inc gold-num)))))
                            gold-num)

                  new-dynamite (if (e/is-pressed? :r) (inc dynamite-num) dynamite-num)
                  _ (when (not= dynamite-num new-dynamite)
                      (>! dynamite new-dynamite))

                  ladder-up? (#{:ladder :ladder-top} square-standing-on)
                  ladder-down? (#{:ladder :ladder-top} square-below)

                  plat (which-platform? old-pos platform-pos platform2-pos platform3-pos)

                  ;; move oldpos by platform movement
                  old-pos (case plat
                            0 old-pos
                            1 (vec2/add old-pos platform-delta)
                            2 (vec2/add old-pos platform2-delta)
                            3 (vec2/add old-pos platform3-delta)
                            nil old-pos)

                  ;; simulate a little vertical move down to see if we are
                  ;; standing on solid ground (or a platform)
                  fallen-pos
                  (constrain-pos platform-constrain
                                 (prepare-platforms platforms fnum old-pos)
                                 old-pos (vec2/add old-pos (vec2/vec2 0 0.1)))

                  ;; TODO: this is dodgy. We need to test with each platform.
                  ;; if we are standing on a platform, we are "bound to it"
                  ;; and we move where it does!
                  standing-on-ground? (> 0.06 (Math/abs (- (vec2/get-y fallen-pos) (vec2/get-y old-pos))))

                  jump-pressed (cond
                                 (and (jump-button-pressed?) (zero? jump-pressed) standing-on-ground?) ;; cant jump off ladder! if you can, problem... when jumping off lader, state stays climbing causing no accel for the jump
                                 (inc jump-pressed)

                                 (and (jump-button-pressed?) (pos? jump-pressed))
                                 (inc jump-pressed)

                                 :default
                                 0)

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
                              ;; zero any y vel in the last frames vel if we are climbing
                              (vec2/set-y (if (= state :climbing) 0 (vec2/get-y old-vel)))
                              ;; no gravity on you when you are on the stairs
                              (vec2/add (if (= state :climbing) (vec2/zero) gravity))

                              (vec2/add jump-force)
                              (vec2/add joy-acc)
                              (vec2/add (vec2/scale player-brake (/ player-vel-x 3)))
                              (vec2/scale 0.98)
                              )

                  new-pos (-> old-pos
                              (vec2/add new-vel))

                  con-pos
                  (constrain-pos platform-constrain
                                 (prepare-platforms platforms fnum old-pos)
                                 old-pos new-pos)

                  old-vel (if (= :walking state) (vec2/sub con-pos old-pos)
                              (-> (vec2/sub con-pos old-pos)
                                  (vec2/set-y 0)))

                  new-dynamite
                  (if (and (pos? new-dynamite) (not last-x-pressed?) (e/is-pressed? :x))
                    (do (make-dynamite dynamites ppos old-vel fnum)
                        (>! dynamite (dec new-dynamite))
                        (dec new-dynamite))
                    new-dynamite)]
              (case facing
                :left (s/set-scale! player -4 4)
                :right (s/set-scale! player 4 4)
                )

              (recur
               next-state
               (inc fnum)
               old-vel
               con-pos
               jump-pressed
               new-gold
               new-dynamite
               (e/is-pressed? :x)
               (case (Math/sign joy-dx)
                 -1 :left
                 0 facing
                 1 :right
                 )
               ))))))))
