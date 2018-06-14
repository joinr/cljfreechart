(ns cljfreechart.charts.core
  (:import  [java.io File]
            [javax.imageio ImageIO]
            [javax.swing JSlider JFrame JLabel JPanel]
            [java.awt BorderLayout Color Shape Rectangle Graphics2D BasicStroke Font]
            [org.jfree.data DomainOrder]
            [org.jfree.data.statistics HistogramDataset
                                       HistogramType
                                       DefaultBoxAndWhiskerCategoryDataset]
            [org.jfree.chart ChartFactory
                             ChartUtilities
                             ChartFrame
                             ChartTheme
                             StandardChartTheme
                             JFreeChart
                             LegendItem
                             LegendItemCollection]
            [org.jfree.chart.axis AxisSpace NumberAxis AxisLocation LogAxis ValueAxis]
            [org.jfree.chart.plot PlotOrientation
                                  DatasetRenderingOrder
                                  SeriesRenderingOrder
                                  Plot
                                  XYPlot]
            [org.jfree.data.xy DefaultHighLowDataset
                               XYSeries
                               XYSeriesCollection
                               AbstractXYDataset
                               DefaultTableXYDataset
                               XYDataItem]
            [org.jfree.data.general Series AbstractDataset]
            [java.awt.Color]
            [org.jfree.data.category DefaultCategoryDataset]
            [org.jfree.data.general DefaultPieDataset]
            [org.jfree.chart.renderer.xy XYLineAndShapeRenderer
                                         XYBarRenderer
                                         XYSplineRenderer
                                         StandardXYBarPainter]
            [org.jfree.chart.renderer PaintScale LookupPaintScale GrayPaintScale]
            [org.jfree.ui TextAnchor RectangleInsets RectangleEdge]
            [org.jfree.chart.title TextTitle]
            [org.jfree.data UnknownKeyException]
            [org.jfree.chart.annotations XYPointerAnnotation
                                         XYTextAnnotation
                                         XYPolygonAnnotation]))

;;this only works for current dataset!
;;revise to work with series-seq....
(defn remove-series
  "Remove an existing series speicified by series-lab.
   If the series does not exist it return nil"
  [chart series-label]
  (let [data-set (-> chart .getPlot .getDataset)
        series   (try (.getSeries data-set series-label)
                      (catch UnknownKeyException e nil))]
    (when series
      (.removeSeries data-set series)
      chart)))

(defn has-series?
  "Test to see if a chart has a series name series-lab"
  [chart series-label]
  (try
    (-> chart .getPlot .getDataset (.getSeries series-label))
    true
    (catch UnknownKeyException e
      false)))

;;review
(defn fire-dataset-changed! [obj]
  (do  (invoke! org.jfree.data.general.AbstractDataset 
                obj 
                fireDatasetChanged)
       obj))

;;operations on plots
;;===================
(defn set-xticks [plot tick]
  (.setTickUnit (.getDomainAxis (.getPlot plot)) (org.jfree.chart.axis.NumberTickUnit. tick)))

;;defining XY series
;;==================
(defn series-by
  "Given key functions for x,y, and series, processes
   collection xs, returning a map of
  {series-key series}
   with meta for ordering purposes:
   {:order {:idx->key [n series-key]
            :key->idx [series-key n]}}"
  [x-key y-key series-key xs]
  (let [order       (atom [])
        get-series! (fn get-series! ^XYSeries [series k] 
                      (if-let [res (get @series k)]
                        res
                        (let [s (XYSeries. k true false)
                              _ (swap! series assoc k s)
                              _ (swap! order (fn [xs] (conj xs [(count xs) k])))]
                          s)))]
    (with-meta   (->> xs
                      (reduce (fn [acc r]
                                (let [^XYSeries s (get-series! acc (series-key r))]
                                  (do (.add s ^double (x-key r) ^double (y-key r))
                                      acc))) (atom {}))
                      (deref))
      (let [os @order]
           {:order {:idx->key   (into {} os)
                    :key->order (into {} (map (fn [[l r]] [r l]) os))}}))))

;;Building XY Datasets
;;====================

;;defining TableXYDatasets for stacked area charts, which is categorically
;;different (hah!) from the CategoryDataset.
(defn ^org.jfree.data.xy.DefaultTableXYDataset build-datatable
  "Creates a table dataset from a series-map (like output from series-by),
   which should be a map of {series-name ^XYSeries obj}
   and uses ordering metadata to add the series in order for the dataset.x"
  [series-map & {:keys [order-by]}]
  (let [{:keys [idx->key key->order]}   (get (meta series-map) :order)
        idxs   (range (count idx->key))
        trends (keys   key->order)
        order  (if order-by 
                 (sort-by order-by trends)  
                 (sort-by (fn [k] (key->order k)) trends))
        addf   (fn [^DefaultTableXYDataset ds ^XYSeries ser]                 
                 (doto ds (.addSeries ser)))]
    (reduce (fn [^DefaultTableXYDataset ds nm] 
              (let [^XYSeries series (get series-map nm) ]
                (addf ds series)))
            (org.jfree.data.xy.DefaultTableXYDataset.)
            order)))

;;we probably want to specify coercions too...
;;Also, avoid having duplicate data, we could
;;define coercions for existing clojure collections
;;and datasets.
(defprotocol IXYData  ;;this should really be XYDataTable
  (^DefaultTableXYDataset as-tablexy [obj])
  (^XYSeries series                  [obj nm])
  (series-seq                        [obj])
  (get-bounds                        [obj]))

(defprotocol IOrderedSeries (order-series-by [obj f]))
(defprotocol IChartable     (as-chart [obj opts]))
(defprotocol IColorable     (set-colors [obj colors]))
(defprotocol IReactiveData  
  (set-notify [obj v])
  (add-samples [plt samples]))

(defn chart [obj] (as-chart obj {}))

(defn vector-ordering [xs]
  (reduce-kv (fn [acc k v]
               (assoc acc v k)) {} xs))

(defn as-order-function [f]
  (cond (vector? f) (let [m (vector-ordering f) bound (inc (count f))] (fn [kv] (get m (first kv) bound))) ;order is implied by position
        (map?    f) (fn [kv] (f (first kv)))   ;order is implied by mapping of series->order
        (fn?     f) f
        :else (throw (Exception. (str "unknown ordering function " f)))))

;;we need generic functions for adding and removing series.

;;show/hide

;;This should be more generic.
;;Do we not have the ability to order any XYDataset by its series?
(defn order-series-by!
  ""
  [^DefaultTableXYDataset tab order-func]
  (let [ofunc (as-order-function order-func)]
        (reduce (fn [^DefaultTableXYDataset acc [nm ^XYSeries ser]]
                  (doto acc (.addSeries ser)))
                (DefaultTableXYDataset.)    
                (sort-by ofunc (series-seq tab)))))

(defn derive-order
  "Given an XYData tab, derives the current order of the series in the
   dataset, returning a bi-directional mapping of idx->series name
   and series-name -> idx."
  [tab]
    (reduce (fn [{:keys [idx->key key->order]} [k ^XYSeries ser]]
              {:idx->key (assoc idx->key (count idx->key) k)
               :key->order (assoc key->order k (count idx->key))})
            {:idx->key {}
             :key->order {}}
            (series-seq tab)))

(declare       stacked-areaxy-chart)

;;there's some weirdness with the duplicate value error, I'm just
;;going to trap it with a try-catch,finally and ignore it.

(extend-type   DefaultTableXYDataset
  IOrderedSeries
  (order-series-by [obj f] (order-series-by! obj f))
  IXYData
  (as-tablexy [obj] obj)
  (series     [obj nm]  (reduce (fn [acc idx] 
                                  (let [^XYSeries ser (.getSeries obj idx)
                                        k (.getKey ser)]
                                    (if (= k nm)
                                      (reduced ser)
                                      acc)))
                                nil 
                                (range (.getSeriesCount obj))))
  (series-seq [obj] (let [cnt (.getSeriesCount obj)]
                      (map (fn [idx] (let [s (.getSeries obj idx)]
                                       [(.getKey s) s]))
                           (range cnt))))
  (get-bounds [obj] (let [^XYSeries s (.getSeries obj 0)]
                      [(.getMinX s) (.getMaxX s)]))
  IReactiveData
  (set-notify [obj v] (let [v (boolean v)]
                        (doseq [[_ ^XYSeries ser] (series-seq obj)]
                          (.setNotify ser v))
                        obj))
  (add-samples [obj samples]
    (let [series-map (into {} (series-seq obj))]
      (do (set-notify obj false)
          (doseq [s samples]
            (doseq [[nm ^double x ^double y] s]
              (.addOrUpdate ^XYSeries (get series-map nm) x y)))  ;;use addOrUpdate to get around false dupes.
          (do! (set-notify obj true))
          (fire-dataset-changed! obj)
          ))))

 
(extend-type   XYSeriesCollection
  IOrderedSeries
  (order-series-by [obj order-func]
    (let [ofunc (as-order-function order-func)]
      (reduce (fn [^XYSeriesCollection acc  [nm ^XYSeries ser]]
                (doto acc (.addSeries ser)))
              (XYSeriesCollection.)    
              (sort-by ofunc (series-seq obj)))))
  IXYData
  (as-tablexy [obj] obj)
  (series     [obj nm]  (.getSeries obj nm))
  (series-seq [obj]     (map (fn [^XYSeries s] [(.getKey s) s]) (seq (.getSeries obj))))
  (get-bounds [obj]    (let [^XYSeries s (.getSeries obj 0)]
                         [(.getMinX s) (.getMaxX s)]))
  IReactiveData
  (set-notify [obj v] (let [v (boolean v)]
                        (doseq [[_ ^XYSeries ser] (series-seq obj)]
                          (.setNotify ser v))
                        obj))
  (add-samples [obj samples]
    (let [series-map (into {} (series-seq obj))]
      (do (set-notify obj false)
          (doseq [s samples]
            (doseq [[nm ^double x ^double y] s]
              (.addOrUpdate ^XYSeries (get series-map nm) x y)))  ;;use addOrUpdate to get around false dupes.

(defrecord xydataset [^DefaultTableXYDataset table seriesmap order] ;this is like a java class
  ;this implements these protocols. A record is just a hash-map record with some fancy java stuff....
  ;Deftype if you don't care if it's a hashmap.
  ;reify and produce a bunch of protocols-Anoymous object that implements a bunch of protocols.
  IOrderedSeries
  (order-series-by [obj f]
    (let [tab  (order-series-by! table f)]
      (xydataset. tab seriesmap (derive-order tab))))
  IXYData
  (as-tablexy [obj] table)
  (series     [obj nm]  (get seriesmap nm))
  (series-seq [obj]     (series-seq table))
  (get-bounds [obj]   (let [^XYSeries s (first (vals seriesmap))]
                           [(.getMinX s) (.getMaxX s)]))
  IChartable 
  (as-chart [obj  opts] (apply stacked-areaxy-chart obj (if (map? opts) 
                                                            (flatten (seq opts))
                                                            opts)))
  IReactiveData
  (set-notify [obj v] (do (set-notify table v) obj))
  (add-samples [obj samples] (do (add-samples table samples) obj)))

(defn clear-series [xyd]
  (do (doseq [[nm ^XYSeries ser] (series-seq xyd)]
        (do! (.clear ser)))
      (fire-dataset-changed! (:table xyd))
      xyd))

(defn get-color [k]
  (let [[r g b] (get spork.graphics2d.canvas/color-specs k)]
    (java.awt.Color. r g b)))


(def ^:dynamic *pallete*
  {:orange         (java.awt.Color. 255 204 51)
   :light-orange   (java.awt.Color. 255 134 36)
   :faded-orange   (java.awt.Color. 255 134 36 100)
   :blue           (java.awt.Color. 51  51  255)
   :light-blue     (java.awt.Color. 102 204 255)
   :yellow         (java.awt.Color. 255 255 0)
   :faded-blue     (java.awt.Color. 102 204 255 100)
   :green          (java.awt.Color. 102 204 0)
   :light-green    (java.awt.Color. 204 255 204)
   :pink           (get-color :pink)
   :red            (get-color :red)
   :black          (get-color :black)
   :amber          (java.awt.Color. 255 204 0)})

(defn color? [c]  (instance?  java.awt.Paint c))
(defn get-color! [c] 
  (if (color? c) c
      (if-let [c (get *pallete* c)]
        (if (keyword? c) (get-color! c)
            c)
        (if-let [c (get spork.graphics2d.canvas/color-specs c)]
          (java.awt.Color. (int (first c)) (int (second c)) (int (nth c 2)))
          (throw (Exception. (str "unknown color " c)))))))        

(extend-type org.jfree.chart.JFreeChart  ;extend-protocol opposite. or extend-type
  IColorable 
  (set-colors [obj colors]
    (let [^org.jfree.chart.plot.XYPlot plot (.getXYPlot ^org.jfree.chart.JFreeChart obj)]      
      (doseq [n  (range (.getDatasetCount plot))]
        (let [ds (.getDataset plot (int n))
              ^org.jfree.chart.renderer.xy.XYItemRenderer xyr (.getRendererForDataset plot ds)]
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

(defn set-domain! [^org.jfree.chart.JFreeChart obj min max]
  (let [^org.jfree.chart.plot.XYPlot plot (.getXYPlot ^org.jfree.chart.JFreeChart obj) ;xyplot is main plot obj
        ax (.getDomainAxis plot)]
    (do (.setRange ax min max)))) ;return obj at the end. same with range

(defn set-range! [^org.jfree.chart.JFreeChart obj min max]
  (let [^org.jfree.chart.plot.XYPlot plot (.getXYPlot ^org.jfree.chart.JFreeChart obj)
        ax (.getRangeAxis plot)]
    (do (.setRange ax min max))))

(defn copy-axes! [^org.jfree.chart.JFreeChart l ^org.jfree.chart.JFreeChart r]
  (let [rx (.getDomainAxis (.getXYPlot r)) 
        ry (.getRangeAxis  (.getXYPlot r))]
    (do (.setRange rx (.getRange (.getDomainAxis (.getXYPlot l))))
        (.setRange ry (.getRange (.getRangeAxis  (.getXYPlot l)))))))

(defn xy-table
  [xkey ykey & options]
    (let [opts         (when options (apply assoc {} options))
          data         (or (:data opts) $data)
          _group-by    (when (:group-by opts) (:group-by opts)) ; (data-as-list (:group-by opts) data)) 
          ;;new
          series-map   (series-by xkey ykey (or _group-by (fn [_] "Series")) (ds/row-maps data))
          ;;new
          order        (get (meta series-map) :order)
          dtbl         (build-datatable   series-map)]
      ;;the difference between the regular area and this guy is that
      ;;we have a category, defined by group-by, and the xs and ys....
      ;;I'll rename _categories to xs at some point and values to
      ;;ys......
      (->xydataset dtbl series-map order)))
