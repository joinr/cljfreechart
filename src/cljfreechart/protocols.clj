;;common/shared protocols for
;;charts.  Provides convenience methods
;;and/or internal implementations for
;;specific data types.
(ns cljfreechart.protocols)

(defprotocol IChartable
  (as-chart [obj opts]))

(def ^:dynamic *pallete* nil)

(defprotocol IChartColor
  (chart-color [x] "coerce x into a color format we can use for charting.
                    Currently only awt is supported as a backend;
                    look into changing implementation options for
                    generality, ala using cljs libs.
                    x should support incanter.color/IColor,
                    providing an rgba encoding in values
                    ranging from [0 255] for each component."))

(extend-protocol IChartColor
  clojure.lang.PersistentVector
  (chart-color [v] (java.awt.Color.  (int (color/get-r v))
                                     (int (color/get-g v))
                                     (int (color/get-b v))
                                     (int (color/get-a v))))
  clojure.lang.Keyword
  (chart-color [k]  (if-not *pallete*
                      (chart-color (color/get-color k))
                      (chart-color (get-pallette k))))
  java.awt.Color
  (chart-color [c] c)
  java.awt.Paint
  (chart-color [p] p)
  clojure.lang.PersistentArrayMap
  (chart-color [m] (chart-color (m :color)))
  clojure.lang.PersistentHashMap
  (chart-color [m] (chart-color (m :color))))

(defprotocol IDatasetMap
  (get-dataset [x k])
  (datasets    [x]))

(defprotocol ISeriesLookup
  (series-in [x d k]))

;;For utility....
;;it might be nice to extend a lookup representation
;;to charts.

;;maybe define a macro for map-like classes...
;;This would allow get-in and the like.
;;Also allow assoc-in ?
;;Maybe not...

;;so, api could be
;(-> chart :plot :datasets  )
;(-> chart :plot :datasets 0)
;(-> chart :plot :datasets 0)

;;another option....
;;we can provide map-like access to the chart object.
;;then define our lookup functions in terms of get-in

(defprotocol ILineSurface
  (add-lines [chart x y & options]))

(defprotocol IChart
  (set-point-size   [chart size opts]
    "Set the point size of a scatter plot. Use series option to apply
     point-size to only one series.")
  (set-stroke-color [chart color opts])
  (set-stroke       [chart color opts])
  (set-theme        [chart theme opts])
  (view-chart       [chart opts])
  (save-chart       [chart filename opts]))

;;we probably want to specify coercions too...
;;Also, avoid having duplicate data, we could
;;define coercions for existing clojure collections
;;and datasets.
(defprotocol IXYData  ;;this should really be XYDataTable
  (^DefaultTableXYDataset as-tablexy [obj])
  (^XYSeries series                  [obj nm])
  (series-seq                        [obj])
  (get-bounds                        [obj]))

(defprotocol IOrderedSeries
  (order-series-by [obj f]))

(defprotocol IColorable
  (set-colors [obj colors]))

(defprotocol IReactiveData
  (set-notify [obj v])
  (add-samples [plt samples]))

(defmulti add-lines* (fn [chart x y & options] (type (get-dataset chart 0))))

;;protocol derived functionality.
#_(defn get-dataset
  ([chart] (get-dataset chart 0))
  ([chart k]
   (p/get-dataset chart k)))

(defn jfree-datasets
  [plot]
  (into {}
        (for [n (range (.getDatasetCount plot))]
          [n (get-dataset plot n)])))

(extend-protocol p/IDataSetMap
  org.jfree.chart.plot.Plot
  (get-dataset [c k] (.. chart (getPlot) (getDataset k)))
  (datasets    [x]   )
  org.jfree.chart.Chart
  (get-dataset [c k] (get-dataset (.getPlot chart) k))
  (datasets    [x]   ))

;;revisit this....
;;protocol function?
;;xyz doesn't have seriesm for instance.
;;If we just allow ourselves to implement series-seq....or
;;some variant that understands JFreeChart items with .getSeries
;;methods are fair game.
(defn has-series? [x]
  (instance? org.jfree.data.xy.XYSeriesCollection x))

;;so...
;;some helper functions.
;;if the classname has SeriesCollection in it, it "should"
;;have a .getSeries method per JFree's design.

;;this is a useful fn, since it helps us with things like
;;global legends and the like.

;;note: we've probably already written this in prock.stacked or the like.
;;note: this doesn't account for subplots.....
;;maybe we need a HOF.
(defn series-details [p]
  (->>  (for [ds  (datasets p)]
          (when (has-series? ds)
            (for [[j xyseries] (map-indexed vector (.getSeries ds))]
              {:dataset     i
               :series      j
               :data        xyseries
               :key         (.getKey xyseries)
               :renderer    (.getRenderer p i)
               :legend-item (.getLegendItem (.getRenderer p i) 0 j)
               })))
        flatten
        (filter identity)))

;;this is a hack to help us get around the clunk in incanter...
(defn append-lines [chart xs ys & options]
  (let [n (-> chart .getPlot .getDatasetCount)
        {:keys [series dataset series-label width dash color points point-size]
         :or {series 0
              dataset n
              series-label (str "series-" n)}} (apply hash-map options)
        chart    (c/add-lines chart xs ys :series-label series-label :points (or points point-size))
        _        (when color
                   (c/set-stroke-color chart color
                                       :series series :dataset dataset ))
        _        (when (or width dash)
                   (c/set-stroke chart
                                 :series series
                                 :dataset dataset
                                 :width (or width 1.0)
                                 :dash (or dash 1.0)))
        _       (when  point-size
                  (c/set-point-size chart point-size
                                 :series  series
                                 :dataset dataset))]
    chart))

;;operations on plots
;;===================
(defn set-xticks [plot tick]
  (.setTickUnit (.getDomainAxis (.getPlot plot)) (org.jfree.chart.axis.NumberTickUnit. tick)))

(defn chart [obj] (as-chart obj {}))

(defn vector-ordering [xs]
  (reduce-kv (fn [acc k v]
               (assoc acc v k)) {} xs))

(defn as-order-function [f]
  (cond (vector? f) (let [m (vector-ordering f)
                          bound (inc (count f))]
                      ;;order is implied by position
                      (fn [kv] (get m (first kv) bound)))
        ;;order is implied by mapping of series->order
        (map?    f) (fn [kv] (f (first kv)))
        (fn?     f) f
        :else (throw (Exception. (str "unknown ordering function " f)))))

;;we need generic functions for adding and removing series.

;;show/hide
;;Note: this works for all ComparableObjectSeries derived types.
(defn clear-series [xyd]
  (do (doseq [[nm ^XYSeries ser] (series-seq xyd)]
        (do! (.clear ser)))
      (fire-dataset-changed! (:table xyd))
      xyd))

;;legacy alias
;;deprecated in favor of chart-color
(defn get-color [k]
  (chart-color k))

;;this may be a bit much....
(extend-type org.jfree.chart.JFreeChart  ;extend-protocol opposite. or extend-type
  IColorable
  (set-colors [obj colors]
    (let [^org.jfree.chart.plot.XYPlot plot
           (.getXYPlot ^org.jfree.chart.JFreeChart obj)]
      (doseq [n  (range (.getDatasetCount plot))]
        (let [ds (.getDataset plot (int n))
              ^org.jfree.chart.renderer.xy.XYItemRenderer xyr
              (.getRendererForDataset plot ds)]
          (doseq  [ [idx [nm ^XYSeries ser]] (map-indexed vector (series-seq ds))]
            (when-let [c (get colors nm)]
              (.setSeriesPaint xyr  (int idx) (get-color! c))))))))
  IXYData
  (as-tablexy [obj] (.getDataset (.getXYPlot obj)))
  (series     [obj nm]  (series (.getDataset (.getXYPlot obj)) nm))
  (series-seq [obj]     (series-seq (.getDataset (.getXYPlot obj))))
  (get-bounds [obj]   (let [^XYSeries s (second (first (series-seq obj)))]
                           [(.getMinX s) (.getMaxX s)])))

(defn xycoords [^XYSeries ser]
  (map (fn [^XYDataItem xy]
         [(.getX xy) (.getY xy)])
       (.getItems ser)))


;;TODO: Generalize!

;;does this only work with XYPlots?
;;Also only works with "Current Dataset"
;;JFreeChart allows defining multiple axes in a plot.
(defn set-domain! [^org.jfree.chart.JFreeChart obj min max]
  (let [^org.jfree.chart.plot.XYPlot plot
           (.getXYPlot ^org.jfree.chart.JFreeChart obj) ;xyplot is main plot obj
        ax (.getDomainAxis plot)]
    (do (.setRange ax min max)))) ;return obj at the end. same with range

;;does this only work with XYPlots?
;;Also only works with "Current Dataset"
;;JFreeChart allows defining multiple axes in a plot.
(defn set-range! [^org.jfree.chart.JFreeChart obj min max]
  (let [^org.jfree.chart.plot.XYPlot plot
           (.getXYPlot ^org.jfree.chart.JFreeChart obj)
        ax (.getRangeAxis plot)]
    (do (.setRange ax min max))))

;;does this only work with XYPlots?
;;Also only works with "Current Dataset"
;;JFreeChart allows defining multiple axes in a plot.
(defn copy-axes! [^org.jfree.chart.JFreeChart l
                  ^org.jfree.chart.JFreeChart r]
  (let [rx (.getDomainAxis (.getXYPlot r))
        ry (.getRangeAxis  (.getXYPlot r))]
    (do (.setRange rx (.getRange (.getDomainAxis (.getXYPlot l))))
        (.setRange ry (.getRange (.getRangeAxis  (.getXYPlot l)))))))

;;OBE
;;this is a bit useless; legacy stuff.
;;we can always filter over datasets...
(defn line-datasets
  [p]
  (vec (datasets p)))
