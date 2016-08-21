(ns tile-map.core
  (:require [infinitelives.pixi.resources :as r]
            [infinitelives.pixi.texture :as t]
            [infinitelives.pixi.canvas :as c]
            [infinitelives.pixi.sprite :as s]
            [infinitelives.utils.events :as e]
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
         "{" [0 48]
         "}" [16 48]
         "[" [0 64]
         "]" [16 64]
         }
        ]
    (->> tile-lookup
         (map (fn [[c pos]] [c (t/sub-texture texture pos [16 16])]))
         (into {}))))

(defn add-tiles! [batch tile-set tile-map]
  (doall
   (for [row (range (count tile-map))
         col (range (count (first tile-map)))]
     (let [char (nth (tile-map row) col)]
       (when (not= " " char)
         (.addChild
          batch
          (s/make-sprite (tile-set char)
                         :x (* 16 col) :y (* 16 row)
                         :xhandle 0 :yhandle 0)))))))

(defn add-bg-tiles! [batch tile-set]
  (doall
   (for [row (range -20 20)
         col (range -20 20)]
     (.addChild
      batch
      (s/make-sprite (tile-set (nth ["{" "}" "[" "]"] (+ (mod col 2) (mod row 2))))
                     :x (* 16 col) :y (* 16 row)
                     :xhandle 0 :yhandle 0)))))

(defonce canvas
  (c/init {:layers [:bg :tilemap :ui]
           :background bg-colour
           :expand true}))

(defonce main
  (go
    ;; load image tilesets
    (<! (r/load-resources canvas :ui
                          [
                           "img/tiles.png"
                           ]))

    ;; make the tile texture lookup
    (let [tile-set (make-tile-set :tiles)
          bg (js/PIXI.ParticleContainer.)
          batch (js/PIXI.ParticleContainer.)
          ]
      (add-tiles! batch tile-set tile-map)
      (add-bg-tiles! bg tile-set)
      (s/set-scale! batch 4)
      (s/set-scale! bg 4)

      ;; nearest
      (m/with-sprite canvas :tilemap
          [
           background bg
           tilemap batch
           ]
          (loop [theta 0]
            (let [x (+ -2000 (* 1000 (Math/sin theta)))
                  y (+ -1000 (* 500 (Math/cos theta)))]
              (s/set-pos! batch (int x) (int y))
              (s/set-pos! background
                          (mod (int (* x 0.90)) (* 4 32))
                          (mod (int (* y 0.90)) ( * 4 32)))
                                        ;; bug
                                        ;(s/set-pos! batch (+ 0.5 (int x)) (+ 0.5 (int y)))
              (<! (e/next-frame))
              (recur (+ theta 0.006))))

))))
