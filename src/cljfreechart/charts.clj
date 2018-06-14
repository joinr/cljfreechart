;;; charts.clj -- Charts library for Clojure built on JFreeChart

;; by David Edgar Liebke http://incanter.org
;; March 11, 2009

;; Copyright (c) David Edgar Liebke, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.htincanter.at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; CHANGE LOG
;; March 11, 2009: First version

(ns ^{:doc "This is the core charting library for Incanter.
            It provides basic scatter plots, histograms, box plots
            xy plots, bar charts, line charts, as well as
            specialized charts like trace plots and Bland-Altman
            plots.

            This library is built on the JFreeChart library
            (http://www.jfree.org/jfreechart/).
            "
       :author "David Edgar Liebke"}
  cljfreechart.charts
  (:require [incanter.core :refer [$ matrix? dataset? vec? to-list plus minus div
                                   group-on bind-columns view save $group-by conj-cols
                                   grid-apply set-data col-names $data sel abs]
             :as core]
        [incanter.stats :refer [quantile quantile-normal cumulative-mean
                               sd correlation variance]]
        [clj-time.coerce :refer [to-date]]
        [cljcolor.core :as color])
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
                             LegendItemSource
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
            [org.jfree.chart.title TextTitle LegendTitle]
            [org.jfree.data UnknownKeyException]
            [org.jfree.chart.annotations XYPointerAnnotation
                                         XYTextAnnotation
                                         XYPolygonAnnotation]))

#_(defmacro xy-plot
  "
  Returns a JFreeChart object representing a xy-plot of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.

  Options:
    :data (default nil) If the :data option is provided a dataset,
                        column names can be used instead of sequences
                        of data as arguments to xy-plot.
    :title (default 'XY Plot') main title
    :x-label (default x expression)
    :y-label (default 'Frequency')
    :legend (default false) prints legend
    :series-label (default x expression)
    :group-by (default nil) -- a vector of values used to group the x and y values into series.
    :points (default false) includes point-markers
    :auto-sort (default true) sort data by x

  See also:
    view, save, add-points, add-lines

  Examples:

    (use '(incanter core stats charts))

    ;; plot the cosine function
    (def x (range -1 5 0.01))
    (def y (cos (mult 2 Math/PI x)))
    (view (xy-plot x y))

    ;; plot gamma pdf with different parameters
    (def x2 (range 0 20 0.1))
    (def gamma-plot (xy-plot x2 (pdf-gamma x2 :shape 1 :scale 2)
                               :legend true
                               :title \"Gamma PDF\"
                               :y-label \"Density\"))
    (view gamma-plot)
    (add-lines gamma-plot x2 (pdf-gamma x2 :shape 2 :scale 2))
    (add-lines gamma-plot x2 (pdf-gamma x2 :shape 3 :scale 2))
    (add-lines gamma-plot x2 (pdf-gamma x2 :shape 5 :scale 1))
    (add-lines gamma-plot x2 (pdf-gamma x2 :shape 9 :scale 0.5))

    ;; use :group-by option
    (use '(incanter core charts datasets))

    (with-data (get-dataset :chick-weight)
      (view (xy-plot :Time :weight :group-by :Chick)))


    ;; see INCANTER_HOME/examples/probability_plots.clj for more examples of plots

  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([]
     `(xy-plot [] [] :x-label "x" :y-label "y"))
  ([x y & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (str '~x))
           y-lab# (or (:y-label opts#) (str '~y))
           series-lab# (or (:series-label opts#)
                           (if group-by#
                             (format "%s, %s (0)" '~x '~y)
                             (format "%s, %s" '~x '~y)))
           args# (concat [~x ~y] (apply concat (seq (apply assoc opts#
                                                           [:group-by group-by#
                                                            :title title#
                                                            :x-label x-lab#
                                                            :y-label y-lab#
                                                            :series-label series-lab#]))))]
       (apply xy-plot* args#))))

#_(defmacro candle-stick-plot
  "
  Produces a candle stick chart

  Options:
    :data (default nil) If the :data option is provided a dataset,
                        column names can be used instead of sequences
                        of data as arguments to xy-plot.
    :date Key for accessing the underlying date series (defaults to :date)
    :high Key for accessing high value data (defaults to :high)
    :low Key for accessing low value data (defaults to :low)
    :open Key for accessing open value data (defaults to :open)
    :close Key for accessing close value data (defaults to :close)
    :volume Key for accessing volume data (defaults to :volume). Volume data is optional
    :title (default 'Candle Stick Plot') main title
    :time-label (default empty)
    :value-label (default empty)
    :legend (default false) prints legend
    :series-label (default empty)

   Example:
     ;; use default mappings so the dataset must have
     ;; :date, :high, :low, :open, :close and :volume keys
     (candle-stick-plot :data <dataset>)
     ;; more customization
     (candle-stick-plot
       :data dataset
       :high :HighPrice
       :low :LowPrice
       :open :StartOfDay
       :close :CoB
       :volume :TransactionVolume
       :legend true
       :time-label \"CoB date\"
       :value-label \"Price\"
       :series-label \"Price time series\"
       :title \"Price information\")
  "
  [& options]
  `(let [opts# ~(when options (apply assoc {} options))
         main-title# (or (:title opts#) "Candle Stick Plot")
         args#
         (concat
          (mapcat #(vector % (or (opts# %) %)) [:volume :high :low :open :close :date])
          (apply concat
                 (seq (apply assoc opts#
                             [:main-title main-title#
                              :time-label (or (opts# :time-label) "")
                              :value-label (or (opts# :value-label) "")
                              :series-label (or (opts# :series-label))]))))]
     (apply candle-stick-plot* args#)))

#_(defmacro time-series-plot
  "
  Returns a JFreeChart object representing a time series plot of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file. Sequence passed in for the x axis should be
  number of milliseconds from the epoch (1 January 1970).

  Options:
    :data (default nil) If the :data option is provided a dataset,
                        column names can be used instead of sequences
                        of data as arguments to xy-plot.
    :title (default '') main title
    :x-label (default x expression)
    :y-label (default y expression)
    :legend (default false) prints legend
    :series-label (default x expression)
    :group-by (default nil) -- a vector of values used to group the x and y values into series.

  See also:
    view, save, add-points, add-lines

  Examples:

    (use '(incanter core stats charts))
    (require '[clj-time.core :refer [date-time]])

    ;; plot numbers against years starting with 1900
    (def dates (map #(-> (date-time (+ 1900 %))
                         .getMillis)
                    (range 100)))
    (def y (range 100))
    (view (time-series-plot dates y
                            :x-label \"Year\"))

  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([x y & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           legend# (or (:legend opts#) false)
           x-lab# (or (:x-label opts#) (str '~x))
           y-lab# (or (:y-label opts#) (str '~y))
           series-lab# (or (:series-label opts#) (if group-by#
                                                   (format "%s, %s (0)" '~x '~y)
                                                   (format "%s, %s" '~x '~y)))
           args# (concat [~x ~y] (apply concat (seq (apply assoc opts#
                                                           [:group-by group-by#
                                                            :title title#
                                                            :legend legend#
                                                            :x-label x-lab#
                                                            :y-label y-lab#
                                                            :series-label series-lab#]))))]
        (apply time-series-plot* args#))))

#_(defmacro scatter-plot
  "
  Returns a JFreeChart object representing a scatter-plot of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.

  Options:
    :title (default '') main title
    :x-label (default x expression)
    :y-label (default 'Frequency')
    :legend (default false) prints legend
    :series-label (default x expression)
    :group-by (default nil) -- a vector of values used to group the x and y values into series.
    :density? (default false) -- chart will represent density instead of frequency.
    :nbins (default 10) -- number of bins (i.e. bars)
    :gradient? (default false) -- use gradient on bars

  See also:
    view, save, add-points, add-lines

  Examples:

    (use '(incanter core stats charts datasets))
    ;; create some data
    (def mvn-samp (sample-mvn 1000 :mean [7 5] :sigma (matrix [[2 1.5] [1.5 3]])))

    ;; create scatter-plot of points
    (def mvn-plot (scatter-plot (sel mvn-samp :cols 0) (sel mvn-samp :cols 1)))
    (view mvn-plot)

    ;; add regression line to scatter plot
    (def x (sel mvn-samp :cols 0))
    (def y (sel mvn-samp :cols 1))
    (def lm (linear-model y x))
    (add-lines mvn-plot x (:fitted lm))

    ;; use :group-by option
    (use '(incanter core stats datasets charts))
    ;; load the :iris dataset
    (def iris (get-dataset :iris))
    ;; plot the first two columns grouped by the fifth column
    (view (scatter-plot ($ :Sepal.Width iris) ($ :Sepal.Length iris) :group-by ($ :Species iris)))

    (view (scatter-plot :Sepal.Length :Sepal.Width :data (get-dataset :iris)))

    (view (scatter-plot :Sepal.Length :Sepal.Width :group-by :Species :data (get-dataset :iris)))

    (with-data (get-dataset :iris)
       (view (scatter-plot :Sepal.Length :Sepal.Width)))

    (with-data (get-dataset :iris)
       (view (scatter-plot :Sepal.Length :Sepal.Width :group-by :Species)))



  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([]
     `(scatter-plot [] [] :x-label "x" :y-label "y"))
  ([x y & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (str '~x))
           y-lab# (or (:y-label opts#) (str '~y))
           series-lab# (or (:series-label opts#) (if group-by#
                                                   (format "%s, %s (0)" '~x '~y)
                                                   (format "%s, %s" '~x '~y)))
           args# (concat [~x ~y] (apply concat (seq (apply assoc opts#
                                                           [:group-by group-by#
                                                            :title title#
                                                            :x-label x-lab#
                                                            :y-label y-lab#
                                                            :series-label series-lab#]))))]
        (apply scatter-plot* args#))))

#_(defn scatter-plot-matrix
  "
  Returns a JFreeChart object displaying a scatter plot matrix for the given data.
  Use the 'view' function to display the chart or 'save' to write it to a file.

  Use:
    (scatter-plot-matrix & options)
    (scatter-plot-matrix data & options)

  Options:
    :data data (default $data) the data set for the plot.
    :title s (default \"Scatter Plot Matrix\").
    :nbins n (default 10) number of bins (ie. bars) in histogram.
    :group-by grp (default nil) name of the column for grouping data.
    :only-first n (default 6) show only the first n most correlating columns of the data set.
    :only-triangle b (default false) shows only the upper triangle of the plot matrix.

  Examples:
    (use '(incanter core stats charts datasets pdf))
    (view (scatter-plot-matrix (get-dataset :iris) :nbins 20 :group-by :Species ))
    (with-data (get-dataset :iris) (view (scatter-plot-matrix :nbins 20 :group-by :Species )))
    (view (scatter-plot-matrix (get-dataset :chick-weight) :group-by :Diet :nbins 20))

    ;;;Input examples for Iris
    ;; Input dataset examples: Incanter data repo, local file, remote file (url)
    (def iris (get-dataset :iris))
    (def iris (read-dataset \"data/iris.dat\" :delim \\space :header true)) ; relative to project home
    (def iris (read-dataset \"https://raw.githubusercontent.com/incanter/incanter/master/data/iris.dat\" :delim \\space :header true))
    ;; Filter dataset to specific columns only
    (def iris ($ [:Sepal.Length :Sepal.Width :Petal.Length :Petal.Width :Species] (get-dataset :iris)))
    (def iris (sel (get-dataset :iris) :cols [:Sepal.Length :Sepal.Width :Petal.Length :Petal.Width :Species]))

    ;;; Scatter plot matrix examples
    ;; Using default options
    (def iris-spm (scatter-plot-matrix iris :group-by :Species))
    ;; filter to metrics only, no categorical dimension for grouping
    (def iris-spm (scatter-plot-matrix :data ($ [:Sepal.Length :Sepal.Width :Petal.Length :Petal.Width] iris)))

    ;; Using more options
    (def iris-spm (scatter-plot-matrix iris
                                       :title \"Iris Scatter Plot Matrix\"
                                       :bins 20 ; number of histogram bars
                                       :group-by :Species
                                       :only-first 4 ; most correlating columns
                                       :only-triangle false))

    ;;;Output examples
    ;; View on Display
    (view iris-spm :width 1280 :height 800)
    ;; Save as PDF
    (save-pdf  iris-spm \"out/iris-spm.pdf\" :width 2560 :height 1600)
    ;; Save as PNG
    (save iris-spm \"out/iris-spm.png\" :width 2560 :height 1600)

    ;; Airline dataset
    (def airline ($ [:year :passengers :month] (read-dataset \"https://raw.github.com/liebke/incanter/master/data/airline_passengers.csv\" :header true)))
    (def airline-spm (scatter-plot-matrix airline  :group-by :month :bins 20 :title \"Airline Scatter Plot Matrix\"))
    (view airline-spm)
    ;; Chick-weight dataset
    (view (scatter-plot-matrix (get-dataset :chick-weight) :group-by :Diet :bins 20 :title \"Chick-weight Scatter Plot Matrix\" ))
  "
  ([& opts] (cond
             (even? (count opts)) (apply scatter-plot-matrix* opts)
             :else (apply scatter-plot-matrix* (apply merge  [:data (first opts)]  (rest opts))))))

#_(defmacro histogram
  "
  Returns a JFreeChart object representing the histogram of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.

  Options:
    :nbins (default 10) number of bins
    :density (default false) if false, plots frequency, otherwise density
    :title (default 'Histogram') main title
    :x-label (default x expression)
    :y-label (default 'Frequency')
    :legend (default false) prints legend
    :series-label (default x expression)


  See also:
    view, save, add-histogram

  Examples:

    (use '(incanter core charts stats))
    (view (histogram (sample-normal 1000)))

    # plot a density histogram
    (def hist (histogram (sample-normal 1000) :density true))
    (view hist)

    # add a normal density line to the plot
    (def x (range -4 4 0.01))
    (add-lines hist x (pdf-normal x))

    # plot some gamma data
    (def gam-hist (histogram (sample-gamma 1000) :density true :nbins 30))
    (view gam-hist)
    (def x (range 0 8 0.01))
    (add-lines gam-hist x (pdf-gamma x))

    (use 'incanter.datasets)
    (def iris (get-dataset :iris))
    (view (histogram :Sepal.Width :data iris))

    (with-data (get-dataset :iris)
      (view (histogram :Petal.Length)))



  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([x & options]
    `(let [opts# ~(if options (apply assoc {} options) {})
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (str '~x))
           series-lab# (or (:series-label opts#) (str '~x))
           args# (concat [~x] (apply concat (seq (apply assoc opts#
                                                        [:title title#
                                                         :x-label x-lab#
                                                         :series-label series-lab#]))))]
        (apply histogram* args#))))

#_(defmacro line-chart
  "
  Returns a JFreeChart object representing a line-chart of the given values and categories.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.

  Arguments:
    categories -- a sequence of categories
    values -- a sequence of numeric values

  Options:
    :title (default '') main title
    :x-label (default 'Categories')
    :y-label (default 'Value')
    :legend (default false) prints legend
    :series-label
    :group-by (default nil) -- a vector of values used to group the values into
                               series within each category.
    :gradient? (default false) -- use gradient on bars


  See also:
    view and save

  Examples:

    (use '(incanter core stats charts datasets))

    (def data (get-dataset :airline-passengers))
    (def years (sel data :cols 0))
    (def months (sel data :cols 2))
    (def passengers (sel data :cols 1))
    (view (line-chart years passengers :group-by months :legend true))
    (view (line-chart months passengers :group-by years :legend true))


    (def seasons (mapcat identity (repeat 3 [\"winter\" \"spring\" \"summer\" \"fall\"])))
    (def years (mapcat identity (repeat 4 [2007 2008 2009])))
    (def x (sample-uniform 12 :integers true :max 100))
    (view (line-chart years x :group-by seasons :legend true))

    (view (line-chart [\"a\" \"b\" \"c\" \"d\" \"e\" \"f\"] [10 20 30 10 40 20]))

    (view (line-chart (sample \"abcdefghij\" :size 10 :replacement true)
                         (sample-uniform 10 :max 50) :legend true))

    ;; add a series label
    (def plot (line-chart [\"a\" \"b\" \"c\"] [10 20 30] :legend true :series-label \"s1\"))
    (view plot)
    (add-categories plot [\"a\" \"b\" \"c\"] [5 25 40] :series-label \"s2\")


    (view (line-chart :year :passengers :group-by :month :legend true :data data))

    (view (line-chart :month :passengers :group-by :year :legend true :data data))

    (with-data data
      (view (line-chart :month :passengers :group-by :year :legend true)))

    (with-data (->> ($rollup :sum :passengers :year (get-dataset :airline-passengers))
                    ($order :year :asc))
      (view (line-chart :year :passengers)))

    (with-data (->> ($rollup :sum :passengers :month (get-dataset :airline-passengers))
                    ($order :passengers :asc))
      (view (line-chart :month :passengers)))


    (with-data ($rollup :sum :passengers :month (get-dataset :airline-passengers))
      (view (line-chart :month :passengers)))



  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([categories values & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (str '~categories))
           y-lab# (or (:y-label opts#) (str '~values))
           series-lab# (or (:series-label opts#) (if group-by#
                                                   (format "%s, %s (0)" '~categories '~values)
                                                   (format "%s, %s" '~categories '~values)))
           args# (concat [~categories ~values] (apply concat (seq (apply assoc opts#
                                                           [:group-by group-by#
                                                            :title title#
                                                            :x-label x-lab#
                                                            :y-label y-lab#
                                                            :series-label series-lab#]))))]
        (apply line-chart* args#))))

#_(defmacro bar-chart
  "
  Returns a JFreeChart object representing a bar-chart of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.

  Arguments:
    categories -- a sequence of categories
    values -- a sequence of numeric values

  Options:
    :title (default '') main title
    :x-label (default 'Categories')
    :y-label (default 'Value')
    :series-label

  :legend (default false) prints legend
    :vertical (default true) the orientation of the plot
    :group-by (default nil) -- a vector of values used to group the values into
                               series within each category.


  See also:
    view and save

  Examples:


    (use '(incanter core stats charts datasets))

    (with-data (get-dataset :co2)
      (view (bar-chart :Type :uptake
                       :title \"CO2 Uptake\"
                       :group-by :Treatment
                       :x-label \"Grass Types\" :y-label \"Uptake\"
                      :legend true)))


    (def data (get-dataset :airline-passengers))
    (view (bar-chart :year :passengers :group-by :month :legend true :data data))

    (with-data  (get-dataset :airline-passengers)
      (view (bar-chart :month :passengers :group-by :year :legend true)))


    (def data (get-dataset :austres))
    (view data)
    (def plot (bar-chart :year :population :group-by :quarter :legend true :data data))
    (view plot)
    (save plot \"/tmp/austres_plot.png\" :width 1000)
    (view \"file:///tmp/austres_plot.png\")


    (def seasons (mapcat identity (repeat 3 [\"winter\" \"spring\" \"summer\" \"fall\"])))
    (def years (mapcat identity (repeat 4 [2007 2008 2009])))
    (def values (sample-uniform 12 :integers true :max 100))
    (view (bar-chart years values :group-by seasons :legend true))

    (view (bar-chart [\"a\" \"b\" \"c\"] [10 20 30]))
    (view (bar-chart [\"a\" \"a\" \"b\" \"b\" \"c\" \"c\" ] [10 20 30 10 40 20]
                     :legend true
                     :group-by [\"I\" \"II\" \"I\" \"II\" \"I\" \"II\"]))

    ;; add a series label
    (def plot (bar-chart [\"a\" \"b\" \"c\"] [10 20 30] :legend true :series-label \"s1\"))
    (view plot)
    (add-categories plot [\"a\" \"b\" \"c\"] [5 25 40] :series-label \"s2\")

    (view (bar-chart (sample \"abcdefghij\" :size 10 :replacement true)
                     (sample-uniform 10 :max 50) :legend true))



  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([categories values & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (str '~categories))
           y-lab# (or (:y-label opts#) (str '~values))
           series-lab# (or (:series-label opts#) (if group-by#
                                                   (format "%s (0)" '~categories)
                                                   (format "%s" '~categories)))
           args# (concat [~categories ~values] (apply concat (seq (apply assoc opts#
                                                           [:group-by group-by#
                                                            :title title#
                                                            :x-label x-lab#
                                                            :y-label y-lab#
                                                            :series-label series-lab#]))))]
        (apply bar-chart* args#))))


#_(defmacro area-chart
  "Returns a JFreeChart object representing an area-chart of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.

  Arguments:
    categories -- a sequence of categories
    values -- a sequence of numeric values

  Options:
    :title (default '') main title
    :x-label (default 'Categories')
    :y-label (default 'Value')
    :series-label
    :legend (default false) prints legend
    :vertical (default true) the orientation of the plot
    :group-by (default nil) -- a vector of values used to group the values into
                               series within each category.


  See also:
    view and save

  Examples:


    (use '(incanter core stats charts datasets))

    (with-data (get-dataset :co2)
      (view (area-chart :Type :uptake
                       :title \"CO2 Uptake\"
                       :group-by :Treatment
                       :x-label \"Grass Types\" :y-label \"Uptake\"
                      :legend true)))


    (def data (get-dataset :airline-passengers))
    (view (area-chart :year :passengers :group-by :month :legend true :data data))

    (with-data  (get-dataset :airline-passengers)
      (view (area-chart :month :passengers :group-by :year :legend true)))


    (def data (get-dataset :austres))
    (view data)
    (def plot (area-chart :year :population :group-by :quarter :legend true :data data))
    (view plot)
    (save plot \"/tmp/austres_plot.png\" :width 1000)
    (view \"file:///tmp/austres_plot.png\")


    (def seasons (mapcat identity (repeat 3 [\"winter\" \"spring\" \"summer\" \"fall\"])))
    (def years (mapcat identity (repeat 4 [2007 2008 2009])))
    (def values (sample-uniform 12 :integers true :max 100))
    (view (area-chart years values :group-by seasons :legend true))

    (view (area-chart [\"a\" \"b\" \"c\"] [10 20 30]))
    (view (area-chart [\"a\" \"a\" \"b\" \"b\" \"c\" \"c\" ] [10 20 30 10 40 20]
                     :legend true
                     :group-by [\"I\" \"II\" \"I\" \"II\" \"I\" \"II\"]))

    ;; add a series label
    (def plot (area-chart [\"a\" \"b\" \"c\"] [10 20 30] :legend true :series-label \"s1\"))
    (view plot)
    (add-categories plot [\"a\" \"b\" \"c\"] [5 25 40] :series-label \"s2\")

    (view (area-chart (sample \"abcdefghij\" :size 10 :replacement true)
                     (sample-uniform 10 :max 50) :legend true))



  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([categories values & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (str '~categories))
           y-lab# (or (:y-label opts#) (str '~values))
           series-lab# (or (:series-label opts#) (if group-by#
                                                   (format "%s (0)" '~categories)
                                                   (format "%s" '~categories)))
           args# (concat [~categories ~values] (apply concat (seq (apply assoc opts#
                                                           [:group-by group-by#
                                                            :title title#
                                                            :x-label x-lab#
                                                            :y-label y-lab#
                                                            :series-label series-lab#]))))]
        (apply area-chart* args#))))

#_(defmacro stacked-area-chart
  "
  Returns a JFreeChart object representing an stacked-area-chart of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.

  Arguments:
    categories -- a sequence of categories
    values -- a sequence of numeric values

  Options:
    :title (default '') main title
    :x-label (default 'Categories')
    :y-label (default 'Value')
    :series-label
    :legend (default false) prints legend
    :vertical (default true) the orientation of the plot
    :group-by (default nil) -- a vector of values used to group the values into
                               series within each category.


  See also:
    view and save

  Examples:


    (use '(incanter core stats charts datasets))

    (with-data (get-dataset :co2)
      (view (stacked-area-chart :Type :uptake
                       :title \"CO2 Uptake\"
                       :group-by :Treatment
                       :x-label \"Grass Types\" :y-label \"Uptake\"
                      :legend true)))


    (def data (get-dataset :airline-passengers))
    (view (stacked-area-chart :year :passengers :group-by :month :legend true :data data))

    (with-data  (get-dataset :airline-passengers)
      (view (stacked-area-chart :month :passengers :group-by :year :legend true)))


    (def data (get-dataset :austres))
    (view data)
    (def plot (stacked-area-chart :year :population :group-by :quarter :legend true :data data))
    (view plot)
    (save plot \"/tmp/austres_plot.png\" :width 1000)
    (view \"file:///tmp/austres_plot.png\")


    (def seasons (mapcat identity (repeat 3 [\"winter\" \"spring\" \"summer\" \"fall\"])))
    (def years (mapcat identity (repeat 4 [2007 2008 2009])))
    (def values (sample-uniform 12 :integers true :max 100))
    (view (stacked-area-chart years values :group-by seasons :legend true))

    (view (stacked-area-chart [\"a\" \"a\" \"b\" \"b\" \"c\" \"c\" ] [10 20 30 10 40 20]
                     :legend true
                     :group-by [\"I\" \"II\" \"I\" \"II\" \"I\" \"II\"]))


  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([categories values & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (str '~categories))
           y-lab# (or (:y-label opts#) (str '~values))
           series-lab# (or (:series-label opts#) (if group-by#
                                                   (format "%s (0)" '~categories)
                                                   (format "%s" '~categories)))
           args# (concat [~categories ~values] (apply concat (seq (apply assoc opts#
                                                           [:group-by group-by#
                                                            :title title#
                                                            :x-label x-lab#
                                                            :y-label y-lab#
                                                            :series-label series-lab#]))))]
        (apply stacked-area-chart* args#))))

#_(defmacro stacked-bar-chart
  "
  Returns a JFreeChart object representing an stacked-bar-chart of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.

  Arguments:
    categories -- a sequence of categories
    values -- a sequence of numeric values

  Options:
    :title (default '') main title
    :x-label (default 'Categories')
    :y-label (default 'Value')
    :series-label
    :legend (default false) prints legend
    :vertical (default true) the orientation of the plot
    :group-by (default nil) -- a vector of values used to group the values into
                               series within each category.


  See also:
    view and save

  Examples:


    (use '(incanter core stats charts datasets))

    (with-data (get-dataset :co2)
      (view (stacked-bar-chart :Type :uptake
                       :title \"CO2 Uptake\"
                       :group-by :Treatment
                       :x-label \"Grass Types\" :y-label \"Uptake\"
                      :legend true)))


    (def data (get-dataset :airline-passengers))
    (view (stacked-bar-chart :year :passengers :group-by :month :legend true :data data))

    (with-data  (get-dataset :airline-passengers)
      (view (stacked-bar-chart :month :passengers :group-by :year :legend true)))


    (def data (get-dataset :austres))
    (view data)
    (def plot (stacked-bar-chart :year :population :group-by :quarter :legend true :data data))
    (view plot)
    (save plot \"/tmp/austres_plot.png\" :width 1000)
    (view \"file:///tmp/austres_plot.png\")


    (def seasons (mapcat identity (repeat 3 [\"winter\" \"spring\" \"summer\" \"fall\"])))
    (def years (mapcat identity (repeat 4 [2007 2008 2009])))
    (def values (sample-uniform 12 :integers true :max 100))
    (view (stacked-bar-chart years values :group-by seasons :legend true))

    (view (stacked-bar-chart [\"a\" \"b\" \"c\"] [10 20 30]))
    (view (stacked-bar-chart [\"a\" \"a\" \"b\" \"b\" \"c\" \"c\" ] [10 20 30 10 40 20]
                     :legend true
                     :group-by [\"I\" \"II\" \"I\" \"II\" \"I\" \"II\"]))

    ;; add a series label
    (def plot (stacked-bar-chart [\"a\" \"b\" \"c\"] [10 20 30] :legend true :series-label \"s1\"))
    (view plot)
    (add-categories plot [\"a\" \"b\" \"c\"] [5 25 40] :series-label \"s2\")

    (view (stacked-bar-chart (sample \"abcdefghij\" :size 10 :replacement true)
                     (sample-uniform 10 :max 50) :legend true))



  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([categories values & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (str '~categories))
           y-lab# (or (:y-label opts#) (str '~values))
           series-lab# (or (:series-label opts#) (if group-by#
                                                   (format "%s (0)" '~categories)
                                                   (format "%s" '~categories)))
           args# (concat [~categories ~values] (apply concat (seq (apply assoc opts#
                                                           [:group-by group-by#
                                                            :title title#
                                                            :x-label x-lab#
                                                            :y-label y-lab#
                                                            :series-label series-lab#]))))]
        (apply stacked-bar-chart* args#))))

#_(defmacro pie-chart
  "
  Returns a JFreeChart object representing a pie-chart of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.

  Arguments:
    categories -- a sequence of categories
    values -- a sequence of numeric values

  Options:
    :title (default '') main title
    :legend (default false) prints legend


  See also:
    view and save

  Examples:


    (use '(incanter core stats charts datasets))

    (view (pie-chart [\"a\" \"b\" \"c\"] [10 20 30]))

     (view (pie-chart (sample \"abcdefghij\" :size 10 :replacement true)
                     (sample-uniform 10 :max 50) :legend true))


     (with-data (->> (get-dataset :hair-eye-color)
                     ($rollup :sum :count [:hair :eye]))
       (view $data)
       (view (pie-chart :hair :count :title \"Hair Color\"))
       (view (pie-chart :eye :count :title \"Eye Color\")))



  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([categories values & options]
    `(let [opts# ~(when options (apply assoc {} options))
           title# (or (:title opts#) "")
           args# (concat [~categories ~values]
                         (apply concat (seq (apply assoc opts#
                                                   [:title title#]))))]
        (apply pie-chart* args#))))


#_(defmacro box-plot
  "
  Returns a JFreeChart object representing a box-plot of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.

  Options:
    :title (default '') main title
    :x-label (default x expression)
    :y-label (default 'Frequency')
    :legend (default false) prints legend
    :series-label (default x expression)
    :group-by (default nil) -- a vector of values used to group the x values into series.

  See also:
    view and save

  Examples:

    (use '(incanter core stats charts))
    (def gamma-box-plot (box-plot (sample-gamma 1000 :shape 1 :scale 2)
                          :title \"Gamma Boxplot\"
                          :legend true))
    (view gamma-box-plot)
    (add-box-plot gamma-box-plot (sample-gamma 1000 :shape 2 :scale 2))
    (add-box-plot gamma-box-plot (sample-gamma 1000 :shape 3 :scale 2))

    ;; use the group-by options
    (use '(incanter core stats datasets charts))
    (with-data (get-dataset :iris)
      (view (box-plot :Petal.Length :group-by :Species :legend true))
      (view (box-plot :Petal.Width :group-by :Species :legend true))
      (view (box-plot :Sepal.Length :group-by :Species :legend true))
      (view (box-plot :Sepal.Width :group-by :Species :legend true)))

    ;; see INCANTER_HOME/examples/probability_plots.clj for more examples of plots

  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([x & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) "")
           y-lab# (or (:y-label opts#) (str '~x))
           series-lab# (or (:series-label opts#) (str '~x))
           category-lab# (or (:category-label opts#) 0)
           args# (concat [~x] (apply concat (seq (apply assoc opts#
                                                        [:group-by group-by#
                                                         :title title#
                                                         :x-label x-lab#
                                                         :y-label y-lab#
                                                         :category-label category-lab#
                                                         :series-label series-lab#]))))]
        (apply box-plot* args#))))

#_(defmacro function-plot
  "
  Returns a xy-plot object of the given function over the range indicated
  by the min-range and max-range arguments. Use the 'view' function to
  display the chart, or the 'save' function to write it to a file.

  Options:
    :title (default '') main title
    :x-label (default x expression)
    :y-label (default 'Frequency')
    :legend (default false) prints legend
    :series-label (default x expression)
    :step-size (default (/ (- max-range min-range) 500))

  See also:
    view, save, add-points, add-lines


  Examples:

    (use '(incanter core stats charts))

    (view (function-plot sin (- Math/PI) Math/PI))
    (view (function-plot pdf-normal -3 3))

    (defn cubic [x] (+ (* x x x) (* 2 x x) (* 2 x) 3))
    (view (function-plot cubic -10 10))

  "
  ([function min-range max-range & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (format "%s < x < %s" '~min-range '~max-range))
           y-lab# (or (:y-label opts#) (str '~function))
           series-lab# (or (:series-label opts#) (format "%s" '~function))
           args# (concat [~function ~min-range ~max-range]
                         (apply concat (seq (apply assoc opts#
                                                   [:group-by group-by#
                                                    :title title#
                                                    :x-label x-lab#
                                                    :y-label y-lab#
                                                    :series-label series-lab#]))))]
       (apply function-plot* args#))))

#_(defmacro parametric-plot
  "
  Returns a xy-plot object of the given parametric function over the range indicated
  by the min-range and max-range arguments. Use the 'view' function to
  display the chart, or the 'save' function to write it to a file.
  Function must take 1 argument - parameter t and return point [x y].

  Options:
    :title (default '') main title
    :x-label (default 'min-x < x < max-x')
    :y-label (default 'min-y < y < max-y')
    :legend (default false) prints legend
    :series-label (default function expression)
    :step-size (default (/ (- max-range min-range) 500))

  See also:
    view, save, add-parametric, function-plot


  Examples:

    (use '(incanter core charts))

    (defn circle [t] [(cos t) (sin t)])
    (view (parametric-plot circle (- Math/PI) Math/PI))

    (defn spiral [t] [(* t (cos t)) (* t (sin t))])
    (view (parametric-plot spiral 0 (* 6 Math/PI)))
  "
  ([function min-range max-range & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           series-lab# (or (:series-label opts#) (format "%s" '~function))
           args# (concat [~function ~min-range ~max-range]
                         (apply concat (seq (apply assoc opts#
                                                   [:group-by group-by#
                                                    :title title#
                                                    :series-label series-lab#]))))]
       (apply parametric-plot* args#))))

#_(defmacro heat-map
  "
  Usage: (heat-map function x-min x-max y-min y-max & options)

  Returns a JFreeChart object representing a heat map of the function across
  the given x and y ranges. Use the 'view' function to display the chart, or
  the 'save' function to write it to a file.  Callers may define the
  number of samples in each direction, and select if they want a
  sparser representation by disabling :auto-scale? .  By default,
  the heat-map will try to scale the 'blocks' or sampled pixels
  to cover the ranges specified.  Depending on the number of
  samples, this may result in a pixelated but performant look.
  Disabling auto-scale? will keep the 'blocks' a constant
  size, leading to potentially sparsely sampled points on
  the surface surrounded by blank regions.

  Arguments:
    function -- a function that takes two scalar arguments and returns a scalar
    x-min    -- lower bound for the first value of the function
    x-max    -- upper bound for the first value of the function
    y-min    -- lower bound for the second value of the function
    y-max    -- upper bound for the second value of the function

  Options:
    :title
    :x-label (default 'x-min < x < x-max')
    :y-label (default 'y-min < y < y-max')
    :z-label -- defaults to function's name
    :color? (default true) -- should the plot be in color or not?
    :include-zero? (default true) -- should the plot include the origin if it
                                     is not in the ranges specified?
    :x-res   (default 100) -- amount of samples to take in the x range
    :y-res   (default 100) -- amount of samples to take in the y range
    :auto-scale? (default true) -- automatically scale the block
                                   width/height to provide a continuous surface
    :discrete-legend? (default false) -- automatically subdivide the legend to match
                                         the discrete color scale vs. fixed subdivisions.
    :grid-lines? (:both)|:vertical|:horizontal -- renders grid-lines for the
                                                heat-map
    :min-z (defaults to minumum z from data) -- lower end of the color-scale
    :max-z (defaults to maximum z from data) -- upper end of the color-scale

  Examples:
    (use '(incanter core charts))
    (defn f [x y] (sin (sqrt (plus (sq x) (sq y)))))
    (view (heat-map f -10 10 -15 15))
    (view (heat-map f -10 10 -10 10 :color? false))
    (view (heat-map f 5 10 5 10 :include-zero? false))

    (defn f2 [x y] (plus (sq x) (sq y)))
    (view (heat-map f2 -10 10 -10 10))
    (view (heat-map f2 -10 10 -10 10 :color? false))

    (use 'incanter.stats)
    (defn f3 [x y] (pdf-normal (sqrt (plus (sq x) (sq y)))))
    (view (heat-map f3 -3 3 -3 3 :x-label \"x1\" :y-label \"x2\" :z-label \"pdf\"))
    (view (heat-map f3 -3 3 -3 3 :color? false))

    (defn f4 [x y] (minus (sq x) (sq y)))
    (view (heat-map f4 -10 10 -10 10))
    (view (heat-map f4 -10 10 -10 10 :color? false))


    (use '(incanter core stats charts))
    (let [data [[0 5 1 2]
                  [0 10 1.9 1]
                  [15 0 0.5 1.5]
                  [18 10 4.5 2.1]]
          diffusion (fn [x y]
                      (sum (map #(pdf-normal (euclidean-distance [x y] (take 2 %))
                                             :mean (nth % 2) :sd (last %))
                                data)))]
      (view (heat-map diffusion -5 20 -5 20)))

  "
  ([function x-min x-max y-min y-max & options]
    `(let [opts# ~(when options (apply assoc {} options))
           x-lab# (or (:x-label opts#) (format "%s < x < %s" '~x-min '~x-max))
           y-lab# (or (:y-label opts#) (format "%s < y < %s" '~y-min '~y-max))
           z-lab# (or (:z-label opts#) (str '~function))
           args# (concat [~function ~x-min ~x-max ~y-min ~y-max]
                         (apply concat (seq (apply assoc opts#
                                                   [:z-label z-lab#
                                                    :x-label x-lab#
                                                    :y-label y-lab#]))))]
       (apply heat-map* args#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  OTHER CHARTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(defn trace-plot
  "
  Returns a trace-plot object, use the 'view' function to display it.

  Options:
    :data (default nil) If the :data option is provided a dataset,
                        a column name can be used instead of a sequence
                        of data for argument x.
    :title (default 'Trace Plot') main title
    :x-label (default 'Iteration')
    :y-label (default 'Value')
    :series-label (default 'Value')

    Examples:
      (use '(incanter core datasets stats bayes charts))
      (def ols-data (to-matrix (get-dataset :survey)))
      (def x (sel ols-data (range 0 2313) (range 1 10)))
      (def y (sel ols-data (range 0 2313) 10))
      (def sample-params (sample-model-params 5000 (linear-model y x :intercept false)))
      (view (trace-plot (:var sample-params)))

      (view (trace-plot (sel (:coefs sample-params) :cols 0)))

  "
  ([x & options]
    (let [opts (when options (apply assoc {} options))
          data (or (:data opts) $data)
          _x (data-as-list x data)
          title (or (:title opts) "Trace Plot")
          x-label (or (:x-label opts) "Iteration")
          y-label (or (:y-label opts) "Value")
          series-lab (or (:series-label opts) "Value")
          theme (or (:theme opts) :default)
          ;legend? (or (:series-label opts) true)
          n (count _x)
          chart (xy-plot (range n)
                         _x ;)]
                         :title title
                         :x-label x-label
                         :y-label y-label
                         :series-label series-lab)]
      (do
        (add-lines chart (range n) (cumulative-mean _x) :series-label "running mean")
        (.setSeriesRenderingOrder (.getPlot chart) SeriesRenderingOrder/FORWARD)
        (.setDatasetRenderingOrder (.getPlot chart) DatasetRenderingOrder/FORWARD)
        (set-theme chart theme)
        chart))))


#_(defn qq-plot
  "
  Returns a QQ-Plot object. Use the 'view' function to display it.

  Options:
    :data (default nil) If the :data option is provided a dataset,
                        a column name can be used instead of a sequence
                        of data for argument x.

  References:
    http://en.wikipedia.org/wiki/QQ_plot

  Examples:

    (use '(incanter core stats charts datasets))
    (view (qq-plot (sample-normal 100)))
    (view (qq-plot (sample-exp 100)))
    (view (qq-plot (sample-gamma 100)))

    (with-data (get-dataset :iris)
      (view (qq-plot :Sepal.Length)))

  "
  ([x & options]
   (let [opts (when options (apply assoc {} options))
         data (or (:data opts) $data)
         _x (data-as-list x data)
         n (count _x)
         quants (for [k (range 1 n)] (/ k (inc n)))
         norm-quants (quantile-normal quants)
         theme (or (:theme opts) :default)
         y (quantile _x :probs quants)]
         (set-theme (scatter-plot norm-quants y
                                  :title "QQ-Plot"
                                  :x-label "Normal theoretical quantiles"
                                  :y-label "Data quantiles"
                                  :series-label "Theoretical Normal")
                    theme))))

#_(defn bland-altman-plot
  "
  Options:
    :data (default nil) If the :data option is provided a dataset,
                        column names can be used instead of sequences
                        of data for arguments x1 and x2.

  Examples:

    (use '(incanter core datasets charts))
    (def flow-meter (to-matrix (get-dataset :flow-meter)))
    (def x1 (sel flow-meter :cols 1))
    (def x2 (sel flow-meter :cols 3))
    (view (bland-altman-plot x1 x2))

    (with-data (get-dataset :flow-meter)
      (view (bland-altman-plot (keyword \"Wright 1st PEFR\")
                               (keyword \"Mini Wright 1st PEFR\"))))



  References:
    http://en.wikipedia.org/wiki/Bland-Altman_plot
    http://www-users.york.ac.uk/~mb55/meas/ba.htm

  "
  ([x1 x2 & options]
    (let [opts (when options (apply assoc {} options))
          data (or (:data opts) $data)
          _x1 (data-as-list x1 data)
          _x2 (data-as-list x2 data)
          plot (scatter-plot (div (plus _x1 _x2) 2) (minus _x1 _x2)
                             :title "Bland Altman Plot"
                             :legend false)
          x-axis (div (plus _x1 _x2) 2)
          y-axis (minus _x1 _x2)
          min-x (reduce min x-axis)
          max-x (reduce max x-axis)
          _x (range min-x max-x (/ (- max-x min-x) 100))
          y-sd (* (sd y-axis) 2)
          theme (or (:theme opts) :default)]
      (do
        (add-lines plot _x (repeat (count _x) 0) :series-label "mean")
        (add-lines plot _x (repeat (count _x) y-sd) :series-label "mean + sd")
        (add-lines plot _x (repeat (count _x) (- 0 y-sd)) :series-label "mean - sd")
        (set-theme plot theme)
        plot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIDER CONTROLS AND PLOTS
(defmacro sliders
  "
  Creates one slider control for each of the given sequence bindings.
  Each slider calls the given expression when manipulated.


  Examples:
    (use '(incanter core stats charts))

    ;; manipulate a normal pdf
    (let [x (range -3 3 0.1)]
      (def pdf-chart (xy-plot))
      (view pdf-chart)
      (sliders [mean (range -3 3 0.1)
                stdev (range 0.1 10 0.1)]
        (set-data pdf-chart [x (pdf-normal x :mean mean :sd stdev)])))


    ;; manipulate a gamma pdf
    (let [x (range 0 20 0.1)]
      (def pdf-chart (xy-plot))
      (view pdf-chart)
      (sliders [scale (range 0.1 10 0.1)
                shape (range 0.1 10 0.1)]
               (set-data pdf-chart [x (pdf-gamma x :scale scale :shape shape)])))



    ;; find the start values of a non-linear model function
    (use '(incanter core charts datasets))
    ;; create model function used in the following data-sorcery post:
    ;; http://data-sorcery.org/2009/06/06/fitting-non-linear-models/

    (defn f [theta x]
      (let [[b1 b2 b3] theta]
        (div (exp (mult (minus b1) x)) (plus b2 (mult b3 x)))))

    (with-data (get-dataset :chwirut)
      (view $data)
      (def chart (scatter-plot ($ :x) ($ :y)))
      (view chart)
      (add-lines chart ($ :x) (f [0 0.01 0] ($ :x)))

      ;; manipulate the model line to find some good start values.
      ;; give the index of the line data (i.e. 1) to set-data.
      (let [x ($ :x)]
        (sliders [b1 (range 0 2 0.01)
                  b2 (range 0.01 2 0.01)
                  b3 (range 0 2 0.01)]
          (set-data chart [x (f [b1 b2 b3] x)] 1))))

  "
  ([[& slider-bindings] body]
     `(let [slider-fn# (fn ~(apply vector (map symbol (take-nth 2 slider-bindings)))
                         (do ~body))
            slider-labels# ~(apply vector (map str (take-nth 2 slider-bindings)))]
        (sliders* slider-fn# ~(apply vector (take-nth 2 (rest slider-bindings))) slider-labels#))))




(defmacro dynamic-xy-plot
  "
  Returns an xy-plot bound to sliders (which tend to appear behind the chart).
  See the sliders macro for more information.


  Examples:

    (use '(incanter core stats charts))

    (let [x (range -3 3 0.1)]
    (view (dynamic-xy-plot [mean (range -3 3 0.1)
                            sd (range 0.1 10 0.1)]
                           [x (pdf-normal x :mean mean :sd sd)]
                           :title \"Normal PDF Plot\")))

   (let [x (range -3 3 0.1)]
     (view (dynamic-xy-plot [mean (range -3 3 0.1)
                             sd (range 0.1 10 0.1)]
            (for [xi x] [xi (pdf-normal xi :mean mean :sd sd)])
            :title \"Normal PDF Plot\")))


  "
  ([[& slider-bindings] expression & options]
     `(let [chart# (xy-plot [] [] ~@options)
            sliders# (sliders ~(vec slider-bindings)
                              (set-data chart# ~expression))]
        (doall sliders#)
        (set-x-label chart# (str '~(first expression)))
        (set-y-label chart# (str '~(second expression))))))


(defmacro dynamic-scatter-plot
  "
  Returns an scatter-plot bound to sliders (which tend to appear behind the chart).
  See the sliders macro for more information.


  Examples:

  (use '(incanter core stats charts))

  (let [x (range -3 3 0.1)]
    (view (dynamic-scatter-plot [mean (range -3 3 0.1)
                                 sd (range 0.1 10 0.1)]
            [x (pdf-normal x :mean mean :sd sd)]
            :title \"Normal PDF Plot\")))


   (let [x (range -3 3 0.1)]
     (view (dynamic-scatter-plot [mean (range -3 3 0.1)
                                  sd (range 0.1 10 0.1)]
            (for [xi x] [xi (pdf-normal xi :mean mean :sd sd)])
            :title \"Normal PDF Plot\")))

  "
  ([[& slider-bindings] expression & options]
     `(let [chart# (scatter-plot [] [] ~@options)
            sliders# (sliders ~(vec slider-bindings)
                              (set-data chart# ~expression))]
        (doall sliders#)
        (set-x-label chart# (str '~(first expression)))
        (set-y-label chart# (str '~(second expression))))))



