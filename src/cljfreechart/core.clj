;;A quick rip of the stacked area chart I built for
;;proc, hopefully we get to make this a module for incanter.
(ns cljfreechart.core
  (:require 
   ;[incanter.core   :refer :all]
   ;[incanter.io     :refer :all]
   ;[incanter.charts :refer :all]
   ;[spork.graphics2d.canvas]
   [clojure.core.matrix.dataset :as ds];;look at this..
   )
  (:import  [org.jfree.data.xy DefaultTableXYDataset 
             XYSeries XYSeriesCollection 
             XYDataItem]
            [org.jfree.data.general Series AbstractDataset]
            [java.awt.Color]))

;;Legacy implementation uses multimethods.

;;these were originally incanter functions.
;;I'd like to step away from incanter in cljfreechart.
;;Maybe we add core.matrix as a dependency and use
;;its dataset, which would be feature-compatible
;;with incanter.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;maybe change this to defn...
(defn view #_org.jfree.chart.JFreeChart
  ([chart & options]
    (let [opts (when options (apply assoc {} options))
          window-title (or (:window-title opts) "Incanter Plot")
          width (or (:width opts) 500)
          height (or (:height opts) 400)
          frame (ChartFrame. window-title chart)]
      (doto frame
        (.setSize width height)
        (.setVisible true))
      frame)))

(defn save #_org.jfree.chart.JFreeChart
  ([chart filename & options]
    (let [opts (when options (apply assoc {} options))
          width (or (:width opts) 500)
          height (or (:height opts) 400)]
      ;; if filename is not a string, treat it as java.io.OutputStream
      (if (string? filename)
        (ChartUtilities/saveChartAsPNG (File. filename) chart width height)
        (ImageIO/write (.createBufferedImage chart width height) "png" filename))
      nil)))


;;These should be protocol fns...

;;; CHART CUSTOMIZATION
(defn set-stroke
  "
  Examples:
    (use '(incanter core charts))

    (doto (line-chart [:a :b :c :d] [10 20 5 35])
      (set-stroke :width 4 :dash 5)
      view)

    (doto (line-chart [:a :b :c :d] [10 20 5 35])
      (add-categories [:a :b :c :d] [20 5 30 15])
      (set-stroke :width 4 :dash 5)
      (set-stroke :series 1 :width 2 :dash 10)
      view)


    (doto (function-plot sin -10 10 :step-size 0.1)
      (set-stroke :width 3 :dash 5)
      view)

    (doto (line-chart [:a :b :c :d] [10 20 5 35])
      (add-categories [:a :b :c :d] [20 5 30 15])
      (set-stroke :series 0 :width 4 :dash 5)
      (set-stroke :series 1 :width 4 :dash 5 :cap java.awt.BasicStroke/CAP_SQUARE))
  "
  ([chart & options]
    (let [{:keys [width dash series dataset cap join]
           :or {width 1.0 dash 1.0 series :all dataset 0
                cap java.awt.BasicStroke/CAP_ROUND
                join java.awt.BasicStroke/JOIN_ROUND}} (apply hash-map options)
          renderer (-> chart .getPlot (.getRenderer dataset))
          stroke (java.awt.BasicStroke. width
                                        cap
                                        join
                                        1.0
                                        (float-array 1.0 dash)
                                        0.0)]
      (if (= :all series)
        (doto renderer
          (.setAutoPopulateSeriesStroke false)
          (.setBaseStroke stroke))
        (.setSeriesStroke renderer series stroke))
      chart)))


(defn set-stroke-color
  "
  Examples:
    (use '(incanter core charts))

    (doto (line-chart [:a :b :c :d] [10 20 5 35])
      (set-stroke :width 4 :dash 5)
      (set-stroke-color java.awt.Color/blue)
      view)

    (doto (xy-plot [1 2 3] [4 5 6])
      (add-points [1 2 3] [4.1 5.1 6.1])
      (set-stroke-color java.awt.Color/black :series 0)
      (set-stroke-color java.awt.Color/red :series 1))

    (doto (function-plot sin -10 10 :step-size 0.1)
      (set-stroke :width 3 :dash 5)
      (set-stroke-color java.awt.Color/gray)
      view)

  "
([chart color & options]
   (let [{:keys [series dataset]
          :or {series 0 dataset 0}} (apply hash-map options)
          renderer (-> chart .getPlot (.getRenderer dataset))]
     (.setSeriesPaint renderer series (chart-color color))
     chart)))

(defn set-point-size
  "Set the point size of a scatter plot. Use series option to apply
  point-size to only one series."
  [chart point-size & {:keys [series dataset] :or {series :all dataset 0}}]
  (let [xy (- (/ point-size 2))
        new-point (java.awt.geom.Ellipse2D$Double. xy xy point-size point-size)
        plot (.getPlot chart)
        series-count (.getSeriesCount plot)
        series-list (if (= :all series)
                      (range 0 series-count)
                      (list series))
        renderer (.getRenderer plot dataset)]
    (doseq [a-series series-list]
      (doto renderer (.setSeriesShape a-series new-point)))
    chart))


;;;; DEFAULT THEME METHODS
(defmethod set-theme-default org.jfree.data.category.DefaultCategoryDataset
  ([chart]
     (let [plot (.getPlot chart)
           renderer (.getRenderer plot)
           bar-painter (org.jfree.chart.renderer.category.StandardBarPainter.)]
       (when (some #{(type (.getRenderer (.getPlot chart)))}
                #{org.jfree.chart.renderer.category.BarRenderer
                  org.jfree.chart.renderer.category.StackedBarRenderer})
         (doto renderer
           (.setBarPainter bar-painter)
           (.setSeriesOutlinePaint 0 java.awt.Color/lightGray)
           (.setShadowVisible false)
           (.setDrawBarOutline false)))
       (set-background-default chart)
       chart)))


(defmethod set-theme-default org.jfree.data.statistics.HistogramDataset
  ([chart]
     (let [plot (.getPlot chart)
           renderer (.getRenderer plot)
           bar-painter (org.jfree.chart.renderer.xy.StandardXYBarPainter.)]
       (doto renderer
         (.setBarPainter bar-painter)
         (.setSeriesOutlinePaint 0 java.awt.Color/lightGray)
         (.setShadowVisible false)
         (.setDrawBarOutline true))
       (set-background-default chart)
       chart)))

(defmethod set-theme-default :default
  ([chart]
     (set-background-default chart)))


;;;; BW THEME METHODS

(defmethod set-theme-bw org.jfree.data.xy.XYSeriesCollection
  ([chart]
     (let [plot (.getPlot chart)
           renderer (.getRenderer plot)]
       (do
          (doto plot
           (.setBackgroundPaint java.awt.Color/white)
           (.setRangeGridlinePaint (java.awt.Color. 235 235 235))
           (.setDomainGridlinePaint (java.awt.Color. 235 235 235)))
         (doto renderer
           (.setOutlinePaint java.awt.Color/white)
           (.setPaint java.awt.Color/gray))
         chart))))


(defmethod set-theme-bw org.jfree.data.statistics.HistogramDataset
  ([chart]
     (let [plot (.getPlot chart)
           renderer (.getRenderer plot)]
       (do
          (doto plot
           (.setBackgroundPaint java.awt.Color/white)
           (.setRangeGridlinePaint (java.awt.Color. 235 235 235))
           (.setDomainGridlinePaint (java.awt.Color. 235 235 235)))
         (doto renderer
           (.setOutlinePaint java.awt.Color/white)
           (.setPaint java.awt.Color/gray))
         chart))))


(defmethod set-theme-bw org.jfree.data.statistics.DefaultBoxAndWhiskerCategoryDataset
  ([chart]
     (let [plot (.getPlot chart)
           renderer (.getRenderer plot)]
       (do
          (doto plot
           (.setBackgroundPaint java.awt.Color/white)
           (.setRangeGridlinePaint (java.awt.Color. 235 235 235))
           (.setDomainGridlinePaint (java.awt.Color. 235 235 235)))
         (doto renderer
           (.setOutlinePaint java.awt.Color/white)
           (.setPaint java.awt.Color/gray))
         chart))))


(defmethod set-theme-bw org.jfree.data.category.DefaultCategoryDataset
  ([chart]
     (let [plot (.getPlot chart)
           renderer (.getRenderer plot)]
       (do
          (doto plot
           (.setBackgroundPaint java.awt.Color/white)
           (.setRangeGridlinePaint (java.awt.Color. 235 235 235))
           (.setDomainGridlinePaint (java.awt.Color. 235 235 235)))
         (doto renderer
           (.setOutlinePaint java.awt.Color/white)
           (.setPaint java.awt.Color/gray))
         chart))))


;;;;; DEFAULT PLOT BACKGROUND SETTINGS
(defmethod set-background-default org.jfree.chart.plot.XYPlot
  ([chart]
     (let [grid-stroke (java.awt.BasicStroke. 1
                                              java.awt.BasicStroke/CAP_ROUND
                                              java.awt.BasicStroke/JOIN_ROUND
                                              1.0
                                              (float-array 2.0 1.0)
                                              0.0)
           plot (.getPlot chart)]
       (doto plot
         (.setRangeGridlineStroke grid-stroke)
         (.setDomainGridlineStroke grid-stroke)
         (.setBackgroundPaint java.awt.Color/lightGray)
         (.setBackgroundPaint (java.awt.Color. 235 235 235))
         (.setRangeGridlinePaint java.awt.Color/white)
         (.setDomainGridlinePaint java.awt.Color/white)
         (.setOutlineVisible false)
         (-> .getDomainAxis (.setAxisLineVisible false))
         (-> .getRangeAxis (.setAxisLineVisible false))
         (-> .getDomainAxis (.setLabelPaint java.awt.Color/gray))
         (-> .getRangeAxis (.setLabelPaint java.awt.Color/gray))
         (-> .getDomainAxis (.setTickLabelPaint java.awt.Color/gray))
         (-> .getRangeAxis (.setTickLabelPaint java.awt.Color/gray))
         ;; (.setDomainMinorGridlinesVisible true)
         ;; (.setRangeMinorGridlinesVisible true)
         (.setDomainZeroBaselineVisible false)
         )
       (if (= (-> plot .getDataset type)
              org.jfree.data.statistics.HistogramDataset)
         (-> plot .getRenderer (.setPaint java.awt.Color/gray)))
       (-> chart .getTitle (.setPaint java.awt.Color/gray))
       chart)))


(defmethod set-background-default org.jfree.chart.plot.CategoryPlot
  ([chart]
     (let [grid-stroke (java.awt.BasicStroke. 1
                                              java.awt.BasicStroke/CAP_ROUND
                                              java.awt.BasicStroke/JOIN_ROUND
                                              1.0
                                              (float-array 2.0 1.0)
                                              0.0)]
       (doto (.getPlot chart)
         (.setRangeGridlineStroke grid-stroke)
         (.setDomainGridlineStroke grid-stroke)
         (.setBackgroundPaint java.awt.Color/lightGray)
         (.setBackgroundPaint (java.awt.Color. 235 235 235))
         (.setRangeGridlinePaint java.awt.Color/white)
         (.setDomainGridlinePaint java.awt.Color/white)
         (.setOutlineVisible false)
         (-> .getDomainAxis (.setAxisLineVisible false))
         (-> .getRangeAxis (.setAxisLineVisible false))
         (-> .getDomainAxis (.setLabelPaint java.awt.Color/gray))
         (-> .getRangeAxis (.setLabelPaint java.awt.Color/gray))
         (-> .getDomainAxis (.setTickLabelPaint java.awt.Color/gray))
         (-> .getRangeAxis (.setTickLabelPaint java.awt.Color/gray))
         ;; (.setRangeMinorGridlinesVisible true)
         )
       (-> chart .getTitle (.setPaint java.awt.Color/gray))
       chart)))

(defmethod set-background-default org.jfree.chart.plot.PiePlot
  ([chart]
     (let [grid-stroke (java.awt.BasicStroke. 1.5
                                              java.awt.BasicStroke/CAP_ROUND
                                              java.awt.BasicStroke/JOIN_ROUND
                                              1.0
                                              (float-array 2.0 1.0)
                                              0.0)]
       (doto (.getPlot chart)
         ;; (.setRangeGridlineStroke grid-stroke)
         ;; (.setDomainGridlineStroke grid-stroke)
         (.setBackgroundPaint java.awt.Color/white)
         (.setShadowPaint java.awt.Color/white)
         (.setLabelShadowPaint java.awt.Color/white)
         (.setLabelPaint java.awt.Color/darkGray)
         (.setLabelOutlinePaint java.awt.Color/gray)
         (.setLabelBackgroundPaint (java.awt.Color. 235 235 235))
         (.setLabelLinksVisible false)
         (.setOutlineVisible false))
       (-> chart .getTitle (.setPaint java.awt.Color/gray))
       chart)))

(defmethod set-background-default :default
  ([chart]
     (let [grid-stroke (java.awt.BasicStroke. 1.5
                                              java.awt.BasicStroke/CAP_ROUND
                                              java.awt.BasicStroke/JOIN_ROUND
                                              1.0
                                              (float-array 2.0 1.0)
                                              0.0)]
       (doto (.getPlot chart)
         ;; (.setRangeGridlineStroke grid-stroke)
         ;; (.setDomainGridlineStroke grid-stroke)
         (.setBackgroundPaint java.awt.Color/lightGray)
         (.setBackgroundPaint (java.awt.Color. 235 235 235))
         ;; (.setRangeGridlinePaint java.awt.Color/white)
         ;; (.setDomainGridlinePaint java.awt.Color/white)
         (.setOutlineVisible false)
         ;; (-> .getDomainAxis (.setAxisLineVisible false))
         ;; (-> .getRangeAxis (.setAxisLineVisible false))
         ;; (-> .getDomainAxis (.setLabelPaint java.awt.Color/gray))
         ;; (-> .getRangeAxis (.setLabelPaint java.awt.Color/gray))
         ;; (-> .getDomainAxis (.setTickLabelPaint java.awt.Color/gray))
         ;; (-> .getRangeAxis (.setTickLabelPaint java.awt.Color/gray))
         ;; (.setRangeMinorGridlinesVisible true)
         )
       (-> chart .getTitle (.setPaint java.awt.Color/gray))
       chart)))


;;assumes chart is a jfeechart, something supporting the
;;.getSubtitle method

(defmulti add-subtitle
  "
  Adds a JFreeChart title object to a chart as a subtitle.

  Examples:
    (use '(incanter core charts latex))

    (doto (function-plot sin -10 10)
      (add-subtitle \"subtitle\")
      (add-subtitle (latex \" \\\\frac{(a+b)^2} {(a-b)^2}\"))
      view)

  "
  (fn [chart title] (type title)))

(defmethod add-subtitle java.awt.image.BufferedImage
  ([chart title]
     (.addSubtitle chart (org.jfree.chart.title.ImageTitle. title))
     chart))

(defmethod add-subtitle java.lang.String
  ([chart title]
     (.addSubtitle chart (org.jfree.chart.title.TextTitle. title))
     chart))

(defmethod add-subtitle :default
  ([chart title]
     (.addSubtitle chart title)
     chart))
