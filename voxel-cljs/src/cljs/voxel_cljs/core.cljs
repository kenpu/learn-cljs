(ns voxel-cljs.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [put! chan <! >! timeout close!]]))
 
; defines the initial scene
(def PI (.-PI js/Math))
(def voxelconf (atom {:x (- (/ PI 4))
                      :y (/ PI 2)
                      :z 0}))

(def size 30)

(defn make-voxel 
  [triple mesh]
  (let [[x y z] triple]
    (new js/voxelcss.Voxel 
         (* size x) (* size y) (* size z) size #js {:mesh mesh})))

(def C '[ 
          (0 0 0)
          (0 0 -1)
          (0 0 1)
          (1 0 -1)
          (2 0 -1)
          (3 0 -1)
          (4 0 0)
          (4 0 -1)
          (4 0 1)
        ])
(def S '[
          (0 0 0)
          (0 0 -1)
          (0 0 1)
          (1 0 1)
          (2 0 -1)
          (2 0 0)
          (2 0 1)
          (3 0 -1)
          (4 0 0)
          (4 0 -1)
          (4 0 1)
        ])

(def DOT '[ (0 0 0) ])

(def U '[ (0 0 0)
          (0 0 -1)
          (0 0 1)
          (1 0 -1)
          (1 0 1)
          (2 0 -1)
          (2 0 1) ])

(def O '[ (0 0  0)
          (0 0 -1)
          (0 0  1)
          (1 0 -1)
          (1 0  1)
          (2 0 -1)
          (2 0  1)
          (2 0  0) 
         ])

(def I '[ (0 0  0)
          (1 0  0)
          (2 0  0) ])

(def T '[ (0 0  0)
          (1 0  0)
          (2 0  0)
          (2 0  -1)
          (2 0  1) ])

(def origin '(0 0 -9))

(def meshes
  [ (.-water js/voxelcss.Meshes) ;; C
    (.-water js/voxelcss.Meshes) ;; S
    (.-dirt js/voxelcss.Meshes) ;; .
    (.-grass js/voxelcss.Meshes) ;; U
    (.-grass js/voxelcss.Meshes) ;; O
    (.-grass js/voxelcss.Meshes) ;; I
    (.-grass js/voxelcss.Meshes) ;; T
   ])


(defn offset-letter [origin i letter]
  (let [offset (* 4 i)
        [x0 y0 z0] origin]
    (for [[x y z] letter]
      [(+ x0 x) (+ y0 y) (+ z0 offset z)])))

(defn layout [offset & letters]
  (map-indexed #(offset-letter origin %1 %2) letters))

(defn uoit-voxels []
  (apply concat 
         (for [[i letter] (map-indexed vector (layout origin C S DOT U O I T))]
            (map #(make-voxel % (meshes i)) letter))))

(defn clear [world]
  (let [voxels (.getVoxels world)]
    (doseq [v voxels]
      (.remove world v))))

(defn init [] 
  (let [scene   (new js/voxelcss.Scene)
        light   (new js/voxelcss.LightSource -100 100 100 950 0.5 1) 
        world   (new js/voxelcss.World scene)
        body    (.-body js/document)]

    ;; setup the world
    ;; and lighting
    (doto scene 
      (.rotate (:x @voxelconf)
               (:y @voxelconf)
               (:z @voxelconf))
      (.attach body)
      (.addLightSource light))

    ;; clear the world
    ;; in case we are doing incremental reloading
    (clear world)

    ;; add voxels to the world
    (doseq [v (uoit-voxels)]
      (.add world v))))

(enable-console-print!)

(defn -main []
  (init))

(-main)
