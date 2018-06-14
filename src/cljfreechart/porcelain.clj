(ns cljfreechart.porcelain)

(defprotocol ILayered
  (add-layer  [obj layer])
  (drop-layer [obj layer])
  (get-layer  [obj layer]))
