(ns tile-map.map
  (:require [infinitelives.utils.console :refer [log]]
            [infinitelives.pixi.resources :as r]
            [infinitelives.pixi.texture :as t]
            [cljs.core.match :refer-macros [match]]))

(def tile-map
  [
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
])

(defn key-for [c]
  (case c
    "-" :dirt
    "X" :stone
    "/" :ladder-top
    "|" :ladder
    "." :ore
    "o" :ore-big
    "b" :gold
    "c" :crate
    "p" :pot
    "0" :web
    " " :space))

(defn strs->keymap [strs]
  (mapv #(mapv key-for %) strs))

(defn change-cell [keymap k v]
  (assoc-in keymap k v))

(defn get-tile-at [tm x y]
  (get-in tm [y x]))

(def not-passable? #{:dirt-top :dirt :dirt-bottom :dirt-top-bottom :stone})
(def passable? (comp not not-passable?))
(def not-walkable? #{:dirt-top :dirt :dirt-bottom :dirt-top-bottom :stone :ladder-top})
(def walkable? (comp not not-walkable?))

(defn remap [y-1 y y+1]
  (match [y-1 y y+1]
         ;; put top-bottom dirt tiles where the dirt is solo
         [(t :guard #{:stone :ladder-top :ladder :crate :pot :web :space}) :dirt (b :guard #{:stone :ladder-top :ladder :crate :pot :web :space})]
         :dirt-top-bottom

         ;; put bottom dirt tiles where the dirt ends
         [_ :dirt (t :guard #{:stone :ladder-top :ladder :crate :pot :web :space})]
         :dirt-bottom

         ;; put top dirt tiles at the top edges
         [(tile :guard #{:stone :ladder-top :ladder :crate :pot :web :space}) :dirt _]
         :dirt-top

         ;; default: dont change tile
         [_ _ _]
         y))

(defn mapv-mapv [ss f]
  (mapv (fn [line]
          (mapv (fn [ch]
                  (f ch))
                line))
        ss))

(defn remap-keymap [keymap]
  (let [height (count keymap)
        width (count (first keymap))]
    (mapv (fn [y]
            (mapv (fn [x]
                    (let [top (get-in keymap [(dec y) x])
                          tile (get-in keymap [y x])
                          bottom (get-in keymap [(inc y) x])]
                      (remap top tile bottom)))
                  (range width)))
           (range height))))

(defn make-tile-set [resource-key]
  (let [texture (r/get-texture resource-key :nearest)
        tile-lookup
        {
         :dirt [0 0]
         :dirt-top [16 0]
         :dirt-bottom [0 16]
         :dirt-top-bottom [16 16]
         :ore [32 0]
         :ore-big [32 16]
         :stone [48 0]
         :web [80 16]
         :ladder-top [64 0]
         :ladder-top-fg [80 0]
         :ladder [64 16]
         :gold [0 32]
         :pot [16 32]
         :crate [32 32]
         }
        ]
    (->> tile-lookup
         (map (fn [[c pos]] [c (t/sub-texture texture pos [16 16])]))
         (into {}))))

(let [tm (remap-keymap
          (change-cell
           (strs->keymap tile-map)
           [0 0] :foo))]
  (log (str
        tm
        "---"
        (mapv-mapv
         tm
         passable?
         ))))
