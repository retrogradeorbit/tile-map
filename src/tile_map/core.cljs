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
"-                                                                   -"
"-            p                                                      -"
"-          OOOOOO                                                   -"
"-OO                                                                 -"
"-     c                                                             -"
"-   OOOOOO     p                                                    -"
"-            OOOOO   b   c    p                                     -"
"-                  XXXX/XXXXXXXXXXXX                                -"
"-TTTTTTTTTTTTTTTTTTX   |                                            -"
"-----.--o----------X   |                                            -"
"-------------o--BBBX   | XXXXX                                      -"
"-BBBBB----------bbb    |  0        b pb          b                  -"
"-     BB--------TTTXX  XXXXXXXX/XXXXXXXXXXX  XXXXXXXXX/X            -"
"-       BBB--------XXbb 0     p|             XXXXXX   |             -"
"-          BBBBBBBBXXXXXXXXXXXX/XXXXXXXXXXXXXX        |             -"
"-                              |                      |             -"
"-                              |                      |             -"
"- p       X   X                |                      |             -"
"-TTTTTTT/TTTTTTTTTT           c|p            p        |             -"
"--------|----------TT c    OOOOOOOO     TTTTTTTTTTTTTTTT            -"
"-BBBBBBB|BBBBBBBBBBB-TT                 ----------------            -"
"-       |           BB-TT p             ----------------            -"
"-       |             B--TTTT  b        ----------------            -"
"-       TTTTTT  p      BB----TTTTT      ----------------            -"
"-b      BBBB--TTT        BB-------O     ----------------            -"
"-OO         --.--          BBBBBBB     T----------------           T-"
"-           ----.T                     -----------------           --"
"- c    b    ------c    p      c        -----------------         TT--"
"-TTTTTTTTTTT------TTTTTTTTTTTTTTTTTTTTT------------------------------"
"---------------------------------------------------------------------"
])

(defn make-tile-set [resource-key]
  (let [texture (r/get-texture resource-key :nearest)
        tile-lookup
        {
         "-" {:pos [0 0] :size [16 16]}
         "T" {:pos [16 0] :size [16 16]}
         "B" {:pos [0 16] :size [16 16]}
         "O" {:pos [16 16] :size [16 16]}
         "." {:pos [32 0] :size [16 16]}
         "o" {:pos [32 16] :size [16 16]}
         "X" {:pos [48 0] :size [16 16]}
         "0" {:pos [48 16] :size [16 16]}
         "/" {:pos [64 0] :size [16 16]}
         "|" {:pos [64 16] :size [16 16]}
         "b" {:pos [0 32] :size [16 16]}
         "p" {:pos [16 32] :size [16 16]}
         "c" {:pos [32 32] :size [16 16]}
         "{" {:pos [0 48] :size [16 16]}
         "}" {:pos [16 48] :size [16 16]}
         "[" {:pos [0 64] :size [16 16]}
         "]" {:pos [16 64] :size [16 16]}
         }
        ]
    (->> tile-lookup
         (map (fn [[c {:keys [pos size]}]] [c (t/sub-texture texture pos size)]))
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
   (for [row (range -20 50)
         col (range -20 50)]
     (do
       (log (nth ["{" "}" "[" "]"] (+ (mod col 2) (mod row 2))))
       (.addChild
                  batch
                  (s/make-sprite (tile-set (nth ["{" "}" "[" "]"] (+ (mod col 2) (mod row 2))))
                                 :x (* 16 col) :y (* 16 row)
                                 :xhandle 0 :yhandle 0))))))

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
            (let [x (+ -1000 (* 500 (Math/sin theta)))
                  y (+ -1000 (* 500 (Math/cos theta)))]
              (s/set-pos! batch (int x) (int y))
              (s/set-pos! background (int (* x 0.75)) (int (* y 0.75)))
                                        ;; bug
                                        ;(s/set-pos! batch (+ 0.5 (int x)) (+ 0.5 (int y)))
              (<! (e/next-frame))
              (recur (+ theta 0.003)))))

      (comment ;; linear with widened textures

        (m/with-sprite canvas :tilemap
          [tilemap batch]
          (loop [x 0 y 0]
            (s/set-pos! batch x y)
                                        ;; bug
                                        ;(s/set-pos! batch (+ 0.5 (int x)) (+ 0.5 (int y)))
            (<! (e/next-frame))
            (recur (- x 0.01) (- y 0.01))))))))
