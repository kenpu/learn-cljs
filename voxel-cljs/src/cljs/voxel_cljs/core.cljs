(ns voxel-cljs.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [put! chan <! >! timeout close!]]
            [voxel-cljs.leap :as leap]))
 
; defines the initial scene
(def PI (.-PI js/Math))
(def voxelconf (atom {;; angle positions
                      :x (- (/ PI 4))
                      :y (/ PI 2)
                      :z 0
                      ;; rotating angles
                      :rx (/ PI 40)
                      :ry (/ PI 50) 
                      :rz (- (/ PI 60))
                      ;; light position
                      :lx -100
                      :ly 100
                      :lz 100
                      ;; light travel distance
                      :ld 950 }))

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
    (.-dirt js/voxelcss.Meshes)  ;; .
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

(defn ticks [delay]
  (let [c (chan)]
    (go-loop [i 0]
      (do
        (<! (timeout delay))
        (>! c i)
        (recur (inc i))))
    c))

(defn init [c] 
  (let [scene   (new js/voxelcss.Scene)
        light   (new js/voxelcss.LightSource 
                     (:lx @voxelconf)
                     (:ly @voxelconf)
                     (:lz @voxelconf)
                     (:ld @voxelconf)
                     0.1 1) 
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
      (.add world v))

    ;; listen the channel for updates
    (go-loop []
             (<! c)
             (let [{:keys [rx ry rz]} @voxelconf]
               (println "doto scene" rx ry rz)
               (doto scene
                 (.rotate rx ry rz))
               (doto light
                 (.setPosition (:lx @voxelconf)
                               (:ly @voxelconf)
                               (:lz @voxelconf))
                 (.setTravelDistance (:ld @voxelconf))))
             (recur))
    ))

(enable-console-print!)

;; computing the new scene / lighting positions
;;
(defn sin [amp freq t]
  (* amp (Math/sin (* freq t))))

(defn get-rotation [t]
  [0 0 0])

(defn get-lighting [t]
  [(+ -0 (sin 200 (/ PI 20) t)) 100 100])

(defn get-lighting-d [t]
  400)

(defn update-chan []
  (let [events (chan)
        clock  (ticks 300)]
    (go-loop []
             (let [t          (<! clock)
                   [rx ry rz] (get-rotation t)
                   [lx ly lz] (get-lighting t)
                   ld         (get-lighting-d t)]
               (swap! voxelconf merge {:rx rx
                                       :ry ry
                                       :rz rz
                                       :lx lx
                                       :ly ly
                                       :lz lz
                                       :ld ld})
               (>! events 1))
               (recur))
    events))

;; -------- leapmotion handler ----------
;;
(defn get-hand-rotation [ev]
  (let [{:keys [normal position]} ev
        [nx ny nz] normal
        scale 0.3]
    (println "leap" nx ny nz)
    (map #(* scale %) [nz nx 0])))

(defn leap-chan 
  []
  (let [hand (leap/events)
        out  (chan)]
    (go-loop [t 0]
             (let [ev (<! hand)
                  [rx ry rz] (get-hand-rotation ev)]
               (swap! voxelconf merge {:rx rx
                                       :ry ry
                                       :rz rz})
               (>! out 1))
             (recur (inc t)))
    out))
                 

(defn -main-1 []
  (let [c (update-chan)]
    (init c)))

(defn -main []
  (let [c (leap-chan)]
        (init c)))

(-main)
