(ns cljfreechart.charts.styling
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
                               AbstractXYDataset]
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

(defmulti set-background-default
  "
  Examples:
    (use '(incanter core stats charts datasets))

    (doto (histogram (sample-normal 1000) :title (str :Test-Tittle))
      set-theme-bw
      view)


    (doto (histogram (sample-normal 1000))
      set-background-default
      (add-histogram (sample-normal 1000 :mean 1))
      view)


    (doto (scatter-plot :speed :dist :data (get-dataset :cars))
      set-theme-bw
      view)

    (doto (scatter-plot :speed :dist :data (get-dataset :cars))
      set-theme-bw
      (set-stroke :dash 5)
      (add-points (plus ($ :speed (get-dataset :cars)) 5) (plus ($ :dist (get-dataset :cars)) 10))
      view)

    (doto (scatter-plot :speed :dist :data (get-dataset :cars))
      set-background-default
      (set-stroke :dash 5)
      (add-function sin 0 25)
      view)


    (doto (xy-plot :speed :dist :data (get-dataset :cars) :legend true)
      set-background-default
      view)


    (doto (scatter-plot :speed :dist :data (get-dataset :cars))
      set-background-default
      view)


    (doto (box-plot (sample-gamma 1000 :shape 1 :scale 2)
                    :legend true)
      view set-background-default
      (add-box-plot (sample-gamma 1000 :shape 2 :scale 2))
      (add-box-plot (sample-gamma 1000 :shape 3 :scale 2)))


    (doto (bar-chart [:a :b :c] [10 20 30] :legend true)
      view
      set-background-default
      (add-categories [:a :b :c] [5 25 40]))


    (doto (line-chart [:a :b :c] [10 20 30] :legend true)
      view
      set-background-default
      (add-categories [:a :b :c] [5 25 40]))

    ;; time-series-plot
    (def epoch 0)
    (defn num-years-to-milliseconds [x]
      (* 365 24 60 60 1000 x))
    (def dates (map num-years-to-milliseconds (range 100)))
    (def chart1 (time-series-plot dates (range 100)))
    (def cw1 (view chart1))
    (add-lines chart1 dates (mult 1/2 (range 100)))

    (def chart2 (time-series-plot (take 10 dates) (mult 1/2 (range 10))))
    (def cw2 (view chart2))
  "
  (fn [chart] (type (.getPlot chart))))

;;note: this only works on single datasets....
(defmulti set-theme-default
  (fn [chart & options] (type (-> chart .getPlot .getDataset))))


(defmulti set-theme-bw
  "

  Examples:
    (use '(incanter core stats charts datasets))

    (doto (histogram (sample-normal 1000))
      set-theme-bw
      view)


    (doto (histogram (sample-normal 1000))
      set-theme-bw
      (add-histogram (sample-normal 1000 :mean 1))
      view)


    (doto (scatter-plot :speed :dist :data (get-dataset :cars))
      set-theme-bw
      view)

    (doto (scatter-plot :speed :dist :data (get-dataset :cars))
      set-theme-bw
      (set-stroke :dash 5)
      (add-points (plus ($ :speed (get-dataset :cars)) 5) (plus ($ :dist (get-dataset :cars)) 10))
      view)

    (doto (scatter-plot :speed :dist :data (get-dataset :cars))
      set-theme-bw
      (set-stroke :dash 5)
      (add-function sin 0 25)
      view)


    (doto (xy-plot :speed :dist :data (get-dataset :cars))
      set-theme-bw
      view)


    (doto (scatter-plot :speed :dist :data (get-dataset :cars))
      set-theme-bw
      (add-lines :speed :dist :data (get-dataset :cars))
      view)


    (doto (box-plot (sample-gamma 1000 :shape 1 :scale 2)
                    :legend true)
      view
      (add-box-plot (sample-gamma 1000 :shape 2 :scale 2))
      (add-box-plot (sample-gamma 1000 :shape 3 :scale 2))
      set-theme-bw)


    (doto (bar-chart [:a :b :c] [10 20 30] :legend true)
      view
      set-theme-bw
      (add-categories [:a :b :c] [5 25 40]))


    (doto (line-chart [:a :b :c] [10 20 30] :legend true)
      view
      set-theme-bw
      (add-categories [:a :b :c] [5 25 40]))


  "
  (fn [chart & options] (type (-> chart .getPlot .getDataset))))

(defn set-theme
  "
  Changes the chart theme.

  Arguments:
    chart -- an Incanter/JFreeChart object
    theme -- either a keyword indicating one of the built-in themes,
             or a JFreeChart ChartTheme object, or a function that
             applies thematic changes to the JFreeChart.
  
  Built-in Themes:
    :default
    :dark

  Examples:

    (use '(incanter core charts))
    (def chart (function-plot sin -4 4))
    (view chart)
    ;; change the theme of chart to :dark
    (set-theme chart :dark)
    ;; change it back to the default
    (set-theme chart :default)

    ;; Example using JFreeTheme
    (use '(incanter core stats charts datasets))

    (import '(org.jfree.chart StandardChartTheme)
            '(org.jfree.chart.plot DefaultDrawingSupplier)
            '(java.awt Color))

    (def all-red-theme
      (doto
        (StandardChartTheme/createJFreeTheme)
        (.setDrawingSupplier
        (proxy [DefaultDrawingSupplier] []
          (getNextPaint [] Color/red)))))

    (def data (get-dataset :airline-passengers))

    (def chart (bar-chart :month :passengers :group-by :year :legend true :data data))

    (doto chart
      ;; has no effect
      (set-theme all-red-theme)
      view)


  References:
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/StandardChartTheme.html
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/ChartTheme.html

  "
  ([chart theme]
     (let [built-in-theme? (some #{theme} #{:dark :legacy :gradient})
           _theme (if built-in-theme?
                    (cond
                     (= theme :dark)
                       (StandardChartTheme/createDarknessTheme)
                     (= theme :legacy)
                       (StandardChartTheme/createLegacyTheme)
                     :default
                       (StandardChartTheme/createJFreeTheme))
                    (cond
                     (= theme :bw)
                       set-theme-bw
                     (instance? ChartTheme theme)
                       #(.apply theme %)
                     (fn? theme)
                       theme
                     :default
                       set-theme-default))
           ;; bar-painter
           ;; (org.jfree.chart.renderer.xy.StandardXYBarPainter.)
           ]
       (do
         (if built-in-theme?
           (do
             (.setShadowVisible _theme false)
             (.apply _theme chart))
           (do
             ;; (doto (-> chart .getPlot .getRenderer)
;;             (.setBarPainter bar-painter)
;;             (.setSeriesOutlinePaint 0 java.awt.Color/lightGray)
;;             (.setShadowVisible false)
;;             (.setDrawBarOutline true))
             (_theme chart)))
         chart))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-alpha
  "
  Sets the alpha level (transparency) of the plot's foreground
  returns the modified chart object.

  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([chart alpha]
    (.setForegroundAlpha (.getPlot chart) alpha)
    chart))

(defn set-background-alpha
  "
  Sets the alpha level (transparency) of the plot's background
  returns the modified chart object.

  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([chart alpha]
    (.setBackgroundAlpha (.getPlot chart) alpha)
    chart))


(defn clear-background
  "
  Sets the alpha level (transparency) of the plot's background to zero
  removing the default grid, returns the modified chart object.

  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([chart]
    (.setBackgroundAlpha (.getPlot chart) 0.0)
    chart))

(defn set-title
  "
  Sets the main title of the plot, returns the modified chart object.

  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([chart title]
    (.setTitle chart title)
    chart))


(defn set-x-label
  "
  Sets the label of the x-axis, returns the modified chart object.

  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([chart label]
    (.setLabel (.getDomainAxis (.getPlot chart)) label)
    chart))


(defn set-y-label
  "
  Sets the label of the y-axis, returns the modified chart object.

  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([chart label]
    (.setLabel (.getRangeAxis (.getPlot chart)) label)
    chart))


(defn set-x-range
  "
  Sets the range of the x-axis on the given chart.

  Examples:

    (use '(incanter core charts datasets))

    (def chart (xy-plot :speed :dist :data (get-dataset :cars)))
    (view chart)
    (set-x-range chart 10 20)

  "
  ([chart lower upper]
     (-> chart
         .getPlot
         .getDomainAxis
         (.setRange lower upper))
     chart))


(defn set-y-range
  "
  Sets the range of the y-axis on the given chart.

  Examples:

    (use '(incanter core charts datasets))

    (def chart (xy-plot :speed :dist :data (get-dataset :cars)))
    (view chart)
    (set-y-range chart 10 60)

  "
  ([chart lower upper]
     (-> chart
         .getPlot
         .getRangeAxis
         (.setRange lower upper))
     chart))


(defn color-grid-lines
  "Alter the grid-line color associated with the chart.
   Whether gridlines show or not depends on visibility."
  [chart color]
  (doto (.getPlot chart)
    (.setDomainGridlinePaint color)
    (.setRangeGridlinePaint  color))
  chart)

(defn set-grid-lines
  "Determines the visibility of the grid-lines.
   grid-lines? may be one of #{nil :both :horizontal :vertical}"
  [chart grid-lines?]
  (doto (.getPlot chart)
    (.setDomainGridlinesVisible  (case grid-lines?
                                       (:both :horizontal) true false))
    (.setRangeGridlinesVisible   (case grid-lines?
                                       (:both :vertical) true false)))
  chart)

(defn black-grid-lines
  "Sets all grid-lines to black, rather than the white
   default theme."
  [chart] (color-grid-lines chart  java.awt.Color/black))


(def positions
  {:right  org.jfree.ui.RectangleEdge/RIGHT
   :left   org.jfree.ui.RectangleEdge/LEFT
   :top    org.jfree.ui.RectangleEdge/TOP
   :bottom org.jfree.ui.RectangleEdge/BOTTOM})

(defn ^org.jfree.ui.RectangleEdge as-position [x]
  (or (when (instance? org.jfree.ui.RectangleEdge x) x)
      (get positions x)
      (throw (ex-info "invalid axis-location!" {:x x}))))

(def axis-locations
  {:bottom-or-left org.jfree.chart.axis.AxisLocation/BOTTOM_OR_LEFT
   :bottom-or-right org.jfree.chart.axis.AxisLocation/BOTTOM_OR_LEFT
   :top-or-left org.jfree.chart.axis.AxisLocation/TOP_OR_LEFT
   :top-or-right org.jfree.chart.axis.AxisLocation/TOP_OR_LEFT})

(defn ^org.jfree.chart.axis.AxisLocation as-axis-location
  [x]
  (or (when (instance? org.jfree.chart.axis.AxisLocation x) x)
      (get axis-locations x)
      (throw (ex-info "invalid axis-location!" {:x x}))))
