; computation is based on http://prcweb.co.uk/lab/d3-tree/
(ns d3-cljs.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljsjs.d3]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]
            [d3-cljs.fractal-tree :as fractal]
            [d3-cljs.events :as events]))

(enable-console-print!)

(def dur-animation 1500)
(def interval 1500)

(def div (js/$ "#main"))

(defn x1 [p] (.-x p))
(defn y1 [p] (.-y p))
(defn x2 [p] (-> p (js->clj :keywordize-keys true) (fractal/end-point) (:x)))
(defn y2 [p] (-> p (js->clj :keywordize-keys true) (fractal/end-point) (:y)))

(defn stroke-width [p] (.-level p))

(declare level)

(defn stroke-opacity [p]
  (condp = (level p)
    1 0.8
    2 0.6
    3 0.3))

(defn stroke-color [p]
  (condp = (level p)
    1 "#777"
    2 "#aaa"
    3 "#080"))

(defn level [p]
  (cond
    (< (.-d p) (/ (:max-depth @fractal/conf) 3))       1
    (< (.-d p) (* 2 (/ (:max-depth @fractal/conf) 3))) 2
    :else                                             3))

(defn draw [tree]
  (do
    (.. js/d3
        (select "svg")
        (selectAll "line")
        (data (clj->js tree))
        (enter)
        (append "line")
        (attr "x1" x1)
        (attr "y1" y1)
        (attr "x2" x2)
        (attr "y2" y2)
        (attr "stroke-width" stroke-width)
        (attr "stroke-opacity" stroke-opacity)
        (attr "stroke" stroke-color)
        (attr "id" #(.-i %)))))

(defn redraw [tree]
  (do
    (.. js/d3
        (select "svg")
        (selectAll "line")
        (data (clj->js tree))
        (transition)
        (duration dur-animation)
        (attr "x1" x1)
        (attr "y1" y1)
        (attr "x2" x2)
        (attr "y2" y2))))

(defn draw-tree [i idx]
  (cond
      (zero? i) (draw (fractal/tree))
      :else     (redraw (fractal/tree))))


(defn update-tree [data idx]
  (let [{x :x y :y} data]
    (fractal/update-conf x y)))








(defn chan-loop [c f]
  (go-loop [idx 0]
       (f (<! c) idx)
       (recur (inc idx))))

(defn run-tree []
  (chan-loop (events/ticks interval 1000) draw-tree))

(defn run-clicks[]
  (chan-loop (events/clicks) update-tree))

(defn -main []
  (do
    (run-tree)
    (run-clicks)))

(-main)
