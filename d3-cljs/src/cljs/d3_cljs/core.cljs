; computation is based on http://prcweb.co.uk/lab/d3-tree/
(ns d3-cljs.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljsjs.d3]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]))

(enable-console-print!)

(def div (js/$ "#main"))

(defn ticks [duration max]
  (let [c (chan)]
    (go
      (loop [i 0]
             (>! c i)
             (<! (timeout duration))
             (if (< i max)
               (recur (inc i))))
      (close! c))
    c))


(def seed {:i 0
           :x 420
           :y 600
           :a 0     ; angle
           :l 130   ; length
           :d 1     ; depth
           })

(def da 0.5)
(def dl 0.76)
(def ar 0.7)
(def max-depth 10)

(def id-counter (atom 0))
(defn get-id []
  (swap! id-counter inc))

(defn sin [x]
  (.sin js/Math x))

(defn cos [x]
  (.cos js/Math x))


(defn rand []
  (.random js/Math))

(defn end-point [b]
  (let [x (+ (:x b) (* (sin (:a b)) (:l b)))
        y (- (:y b) (* (cos (:a b)) (:l b)))]
    {:x x
     :y y}))

(defn make-left-branch [b]
  (let [end (end-point b)
        daR (- (* ar (rand)) (* ar 0.5))]
    {:i (get-id)
     :x (:x end)
     :y (:y end)
     :a (+ (- (:a b) da) daR)
     :l (* (:l b) dl)
     :d (+ (:d b) 1)
     :parent (:i b)}))


(defn make-right-branch [b]
  (let [end (end-point b)
        daR (- (* ar (rand)) (* ar 0.5))]
    {:i (get-id)
     :x (:x end)
     :y (:y end)
     :a (+ (+ (:a b) da) daR)
     :l (* (:l b) dl)
     :d (+ (:d b) 1)
     :parent (:i b)}))

(defn get-x [d]
  (.-x d))

(defn get-y [d]
  (.-y d))

(defn get-endx [d]
  (-> d (js->clj :keywordize-keys true) end-point :x ))

(defn get-endy [d]
  (-> d (js->clj :keywordize-keys true) end-point :y))

(defn get-width [d]
  (- (inc max-depth) (.-d d)))

(defn level [d]
  (cond
    (< (.-d d) (/ max-depth 3)) 1
    (< (.-d d) (* 2 (/ max-depth 3))) 2
    :else 3))

(defn get-color [d]
  (condp = (level d)
    1 "#888"
    2 "#8a8"
    3 "#080"))

(defn get-opacity [d]
  (condp = (level d)
    1 0.8
    2 0.6
    3 0.3))

(defn grow [b]
  (if (= (:d b) max-depth)
    [b]
    (let [b-left (make-left-branch b)
          b-right (make-right-branch b)]
      (concat [b] (grow b-left) (grow b-right)))))

(defn fractal-tree []
  (do
    (reset! id-counter 0)
    (grow seed)))

(defn draw-fractal-tree [branches]
  (do
    (.. js/d3 
        (select "svg")
        (selectAll "line")
        (data (clj->js branches))
        (enter)
        (append "line")
        (attr "x1" get-x)
        (attr "y1" get-y)
        (attr "x2" get-endx)
        (attr "y2" get-endy)
        (style "stroke-width" get-width)
        (style "stroke" get-color)
        (style "stroke-opacity" get-opacity)
        (attr "id" get-id))))

(defn update-fractal-tree [branches]
  (do
    (.. js/d3
        (select "svg")
        (selectAll "line")
        (data (clj->js branches))
        (transition)
        (duration 5000)
        (attr "x1" get-x)
        (attr "y1" get-y)
        (attr "x2" get-endx)
        (attr "y2" get-endy))))

(defn -main []
  (let [c (ticks 5000 100000)]
    (go
      (loop []
        (let [i (<! c)]
          (cond
            (zero? i) (draw-fractal-tree (fractal-tree))
            :else (update-fractal-tree (fractal-tree)))
          (if (-> i nil? not) (recur)))))))

(-main)
