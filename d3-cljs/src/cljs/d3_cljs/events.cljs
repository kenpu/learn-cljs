(ns d3-cljs.events
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [put! chan <! >! timeout close!]]))

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

(defn e->data [w h e]
  (let [ex (.-clientX e)
        ey (.-clientY e)]
    {:x (* 2 (- (/ ex w) 0.5))
     :y (/ ey h)}))

(defn clicks []
  (let [c (chan)
        body (js/$ "body")
        w (.width body)
        h (.height body)]
      (. body on "click" #(go (put! c (e->data w h %))))
      c))

