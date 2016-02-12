(ns voxel-cljs.leap
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])
    (:require [cljs.core.async :refer [put! chan <! >! timeout close!]]))

(defn consume-frame [out frame]
  (go 
    (let [hands [.-hands frame]]
      (if (> (count hands) 0)
        (let [hand (first hands)
              normal (.-palmNormal hand)
              position (.-palmPosition hand)]
          (>! out {:position position
                   :normal   normal}))))))

(defn events []
  (let [c (chan)]
    (.loop js/Leap #(consume-frame c %))
    c))

