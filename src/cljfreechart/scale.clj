(ns cljfreechart.scale)

(defprotocol IPaintScale
  (as-paint-scale [x]))

(defn ->paint-scale
  "Constructs a JFREE Chart paint scale from the parameters, to be
   used with an XYPlot renderer."
  ([min max colors]
   (org.jfree.chart.renderer.LookupPaintScale. min max java.awt.Color/white))
  ([min max]
   (org.jfree.chart.renderer.GrayPaintScale. min max)))


;;a paint-scale, in jfreechart parlance, is a scale that maps
;;a continuous range of values onto a discrete sequence of
;;categories, where the categories have an associated java.awt.Paint.
;;It provides
;;get-lower :: unit   -> Double
;;get-upper :: unit   -> Double
;;get-paint :: double -> java.awt.Paint
;;for use as a pluggable color palette with the xyblockrenderer.

;;so, you get a lookup table
;;mapping [0 1.0] to [0 n]
;;for each n in [n color]

(defn add-lookup-color [^LookupPaintScale scale v color]
  (doto scale
    (.add (double v)
          ^java.awt.Paint (chart-color color))))


;;I'd like an easy way to define these;
#_{0        :green
   20       :blue
   50       :red
   :default :black}

;;currently wrapping jfree because that's what our default is...
(defn map->color-scale [m]
  (let [default (get m :default :white)
        entries (vec (sort-by key (dissoc m :default)))
        lower     (key (first entries))
        upper     (key (last entries))
        ^LookupPaintScale scale
          (org.jfree.chart.renderer.LookupPaintScale.
           (double lower)
           (double upper)
           ^java.awt.Paint (chart-color default))]
    (reduce (fn [acc [n paint]]
              (add-lookup-color acc n (chart-color paint)))
            scale entries)))

(defn ->gray-paint-scale [min-val max-val]
  (org.jfree.chart.renderer.GrayPaintScale. min-val max-val))

(defn derive-color-scale [colors min-z max-z color?]
  (let [scale (if (map? colors)  (map->color-scale colors #_min-z #_max-z)
                  (if color?
                    (map->color-scale (into {}  (spread-doubles min-z max-z colors)))
                    #_(org.jfree.chart.renderer.LookupPaintScale. min-z max-z java.awt.Color/white)
                    (org.jfree.chart.renderer.GrayPaintScale. min-z max-z)))
        ]
    scale
    ))
