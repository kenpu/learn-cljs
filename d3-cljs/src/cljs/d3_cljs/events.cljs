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


 

