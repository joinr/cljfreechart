(ns cljfreechart.charts.xy
  (:require [cljfreechart.protocols :as p]))

;;we run into this ALOT.
;;The chart type is typically embedded in the plot/dataset,
;;EXCEPT when it's not
(defn get-datasets
  [plot]
  (let [n (.getDatasetCount plot)]
    (mapv #(get-dataset plot 

;;we need to regine this to allow more datasets 
(defmulti add-lines* (fn [chart x y & options] (type (get-dataset chart))))

(defmethod add-lines* org.jfree.data.xy.XYSeriesCollection
  ([chart x y & options]
     (let [opts (when options (apply assoc {} options))
           data (or (:data opts) $data)
           _x (data-as-list x data)
           _y (data-as-list y data)
           data-plot (.getPlot chart)
           n (.getDatasetCount data-plot)
           series-lab (or (:series-label opts) (format "%s, %s" 'x 'y))
           data-series (XYSeries. series-lab (:auto-sort opts true))
           points? (true? (:points opts))
           line-renderer (XYLineAndShapeRenderer. true points?)
           ;; data-set (.getDataset data-plot)
           data-set (XYSeriesCollection.)]
       (dorun
        (map (fn [x y]
               (if (and (not (nil? x))
                        (not (nil? y)))
                 (.add data-series (double x) (double y))))
             _x _y))
      (.addSeries data-set data-series)
      (doto data-plot
        (.setSeriesRenderingOrder org.jfree.chart.plot.SeriesRenderingOrder/FORWARD)
        (.setDatasetRenderingOrder org.jfree.chart.plot.DatasetRenderingOrder/FORWARD)
        (.setDataset n data-set)
        (.setRenderer n line-renderer))
      chart)))

(defn extend-line
  " Add new data set to an exiting series if it already exists,
    otherwise, data set will be added to a newly created series. "
  [chart x y & options]
  (let [opts       (when options (apply assoc {} options))
        data       (or (:data opts) $data)
        _x         (data-as-list x data)
        _y         (data-as-list y data)
        series-lab (or (:series-label opts) (format "%s, %s" 'x 'y))
        data-set   (-> chart .getPlot .getDataset)
        series     (try (.getSeries data-set series-lab)
                        (catch UnknownKeyException e
                          (let [new-series    (XYSeries. series-lab (:auto-sort opts true))
                                ;; line-renderer (XYLineAndShapeRenderer. true (true? (:points opts)))
                                ]
                            (.addSeries data-set new-series)
                            new-series)))]
    (dorun
     (map (fn [x y]
            (if (and (not (nil? x))
                     (not (nil? y)))
              (.add series (double x) (double y))))
          _x _y))
    chart))

;; doesn't work
(defmethod add-lines* org.jfree.data.statistics.HistogramDataset
  ([chart x y & options]
     (let [opts (when options (apply assoc {} options))
           data (or (:data opts) $data)
           _x (data-as-list x data)
           _y (data-as-list y data)
           data-plot (.getPlot chart)
           n (.getDatasetCount data-plot)
           series-lab (or (:series-label opts) (format "%s, %s" 'x 'y))
           data-series (XYSeries. series-lab)
           points? (true? (:points opts))
           line-renderer (XYLineAndShapeRenderer. true points?)
           data-set (XYSeriesCollection.)]
       (dorun
        (map (fn [x y]
               (if (and (not (nil? x))
                        (not (nil? y)))
                 (.add data-series (double x) (double y))))
             _x _y))
       (.addSeries data-set data-series)
       (doto data-plot
         (.setSeriesRenderingOrder org.jfree.chart.plot.SeriesRenderingOrder/FORWARD)
         (.setDatasetRenderingOrder org.jfree.chart.plot.DatasetRenderingOrder/FORWARD)
         (.setDataset n data-set)
         (.setRenderer n line-renderer))
       chart)))

(defmacro add-lines
  "
  Plots lines on the given scatter or line plot (xy-plot) of the (x,y) points.
  Equivalent to R's lines function, returns the modified chart object.

  Options:
    :series-label (default x expression)
    :points (default false)
    :auto-sort (default true) sort data by x


  Examples:

    (use '(incanter core stats io datasets charts))
    (def cars (to-matrix (get-dataset :cars)))
    (def y (sel cars :cols 0))
    (def x (sel cars :cols 1))
    (def plot1 (scatter-plot x y :legend true))
    (view plot1)

    ;; add regression line to scatter plot
    (def lm1 (linear-model y x))
    (add-lines plot1 x (:fitted lm1))

    ;; model the data without an intercept
    (def lm2 (linear-model y x :intercept false))
    (add-lines plot1 x (:fitted lm2))


    ;; Clojure's doto macro can be used to build a chart
    (doto (histogram (sample-normal 1000) :density true)
          (add-lines (range -3 3 0.05) (pdf-normal (range -3 3 0.05)))
          view)


    (with-data (get-dataset :iris)
        (doto (xy-plot :Sepal.Width :Sepal.Length :legend true)
              (add-lines :Petal.Width :Petal.Length)
              view))



  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html


  "
  ([chart x y & options]
    `(let [opts# ~(when options (apply assoc {} options))
           series-lab# (or (:series-label opts#)
                           (format "%s, %s" '~x '~y))
           args# (concat [~chart ~x ~y] (apply concat (seq (apply assoc opts#
                                                                  [:series-label series-lab#]))))]
        (apply add-lines* args#))))



(defn add-function*
  ([chart function min-range max-range & options]
    (let [opts (when options (apply assoc {} options))
           step-size (or (:step-size opts)
                         (float (/ (- max-range min-range) 500)))
           x (range-inclusive min-range max-range step-size)
           series-lab (or (:series-label opts)
                          (format "%s" 'function))]
       (add-lines chart x (map function x) :series-label series-lab))))


(defmacro add-function
  "
  Adds a xy-plot of the given function to the given chart, returning
  a modified version of the chart.

  Options:
    :series-label (default x expression)
    :step-size (default (/ (- max-range min-range) 500))

  See also:
    function-plot, view, save, add-function, add-points, add-lines


  Examples:

    (use '(incanter core stats charts))

    ;; plot the sine and cosine functions
    (doto (function-plot sin (- Math/PI) Math/PI)
          (add-function cos (- Math/PI) Math/PI)
          view)


    ;; plot two normal pdf functions
    (doto (function-plot pdf-normal -3 3 :legend true)
          (add-function (fn [x] (pdf-normal x :mean 0.5 :sd 0.5)) -3 3)
          view)


    ;; plot a user defined function and its derivative
    (use '(incanter core charts optimize))

    ;; define the function, x^3 + 2x^2 + 2x + 3
    (defn cubic [x] (+ (* x x x) (* 2 x x) (* 2 x) 3))

    ;; use the derivative function to get a function
    ;; that approximates its derivative
    (def deriv-cubic (derivative cubic))

    ;; plot the cubic function and its derivative
    (doto (function-plot cubic -10 10)
          (add-function deriv-cubic -10 10)
          view)

  "
  ([chart function min-range max-range & options]
    `(let [opts# ~(when options (apply assoc {} options))
           series-lab# (or (:series-label opts#) (str '~function))
           args# (concat [~chart ~function ~min-range ~max-range]
                         (apply concat (seq (apply assoc opts#
                                                   [:series-label series-lab#]))))]
        (apply add-function* args#))))


(defn add-parametric*
  ([chart function min-range max-range & options]
    (let [opts (when options (apply assoc {} options))
          step-size (or (:step-size opts)
                        (float (/ (- max-range min-range) 500)))
          t (range-inclusive min-range max-range step-size)
          [x y] (apply map vector (map function t))
          series-lab (or (:series-label opts)
                         (format "%s" 'function))]
       (add-lines chart x y :series-label series-lab :auto-sort false))))


(defmacro add-parametric
  "
  Adds a xy-plot of the given parametric function to the given chart, returning
  a modified version of the chart.
  Function takes 1 argument t and returns point [x y].

  Options:
    :series-label (default function expression)
    :step-size (default (/ (- max-range min-range) 500))

  See also:
    parametric-plot, view, save, add-function, add-points, add-lines


  Examples:

    (use '(incanter core charts))

    ;;; Plot square with circle inside.
    (defn circle [t] [(cos t) (sin t)])
    (doto (xy-plot [1 -1 -1 1 1] [1 1 -1 -1 1] :auto-sort false)
          (add-parametric circle 0 (* 2 Math/PI))
          (view))
  "
  ([chart function min-range max-range & options]
    `(let [opts# ~(when options (apply assoc {} options))
           series-lab# (or (:series-label opts#) (str '~function))
           args# (concat [~chart ~function ~min-range ~max-range]
                         (apply concat (seq (apply assoc opts#
                                                   [:series-label series-lab#]))))]
        (apply add-parametric* args#))))


(defn add-points*
  ([chart x y & options]
     (let [opts (when options (apply assoc {} options))
           data (or (:data opts) $data)
           _x (data-as-list x data)
           _y (data-as-list y data)
           data-plot (.getPlot chart)
           n (.getDatasetCount data-plot)
           series-lab (or (:series-label opts) (format "%s, %s" 'x 'y))
           data-series (XYSeries. series-lab)
           line-renderer (XYLineAndShapeRenderer. false true)
           data-set (XYSeriesCollection.)]
       (dorun
        (map (fn [x y]
               (if (and (not (nil? x))
                        (not (nil? y)))
                 (.add data-series (double x) (double y))))
             _x _y))
       (.setSeriesRenderingOrder (.getPlot chart) org.jfree.chart.plot.SeriesRenderingOrder/FORWARD)
       (.setDatasetRenderingOrder (.getPlot chart) org.jfree.chart.plot.DatasetRenderingOrder/FORWARD)
       (.addSeries data-set data-series)
       (.setDataset data-plot n data-set)
       (.setRenderer data-plot n line-renderer)
       chart)))

(defmacro add-points
  "
  Plots points on the given scatter-plot or xy-plot of the (x,y) points.
  Equivalent to R's lines function, returns the modified chart object.

  Options:
    :series-label (default x expression)

  Examples:

    (use '(incanter core stats io datasets charts))
    (def cars (to-matrix (get-dataset :cars)))
    (def y (sel cars :cols 0))
    (def x (sel cars :cols 1))

    ;; add regression line to scatter plot
    (def lm1 (linear-model y x))
    ;; model the data without an intercept
    (def lm2 (linear-model y x :intercept false))

    (doto (xy-plot x (:fitted lm1) :legend true)
          view
          (add-points x y)
          (add-lines x (:fitted lm2)))


    (with-data (get-dataset :iris)
      (doto (scatter-plot :Sepal.Length :Sepal.Width :data ($where {:Species \"setosa\"}))
            (add-points :Sepal.Length :Sepal.Width :data ($where {:Species \"versicolor\"}))
            (add-points :Sepal.Length :Sepal.Width :data ($where {:Species \"virginica\"}))
            view))

    ;; of course this chart can be achieved in a single line:
    (view (scatter-plot :Sepal.Length :Sepal.Width :group-by :Species :data (get-dataset :iris)))



  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html


  "
  ([chart x y & options]
    `(let [opts# ~(when options (apply assoc {} options))
           series-lab# (or (:series-label opts#) (format "%s, %s" '~x '~y))
           args# (concat [~chart ~x ~y] (apply concat (seq (apply assoc opts#
                                                           [:series-label series-lab#]))))]
        (apply add-points* args#))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  NEW CHART FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- create-xy-plot
  [title x-lab y-lab dataset legend? tooltips? urls?]
  (org.jfree.chart.ChartFactory/createXYLineChart
    title
    x-lab
    y-lab
    dataset
    org.jfree.chart.plot.PlotOrientation/VERTICAL
    legend?
    tooltips?
    urls?))


(defn- create-xy-series-plot
  ([x y create-plot & options]
     (let [opts (when options (apply assoc {} options))
          data (or (:data opts) $data)
          _x (data-as-list x data)
          _y (data-as-list y data)
          _group-by (when (:group-by opts)
                      (data-as-list (:group-by opts) data))
          x-groups (when _group-by
                     (let [x-groupped (->> (conj-cols _x _group-by)
                                           ($group-by 1))]
                       (->> (distinct _group-by)
                            (map #(->>
                                   (get x-groupped {1 %})
                                   ($ 0))))))
           y-groups (when _group-by
                     (let [y-groupped (->> (conj-cols _y _group-by)
                                           ($group-by 1))]
                       (->> (distinct _group-by)
                            (map #(->>
                                   (get y-groupped {1 %})
                                   ($ 0))))))
           __x (in-coll (if x-groups (first x-groups) _x))
           __y (in-coll (if y-groups (first y-groups) _y))
           title (or (:title opts) "")
           x-lab (or (:x-label opts) (str 'x))
           y-lab (or (:y-label opts) (str 'y))
           series-lab (or (:series-label opts)
                          (if x-groups
                            (format "%s, %s (0)" 'x 'y)
                            (format "%s, %s" 'x 'y)))
           theme (or (:theme opts) :default)
           legend? (true? (:legend opts))
           points? (true? (:points opts))
           data-series (XYSeries. (cond
                                   _group-by
                                     (first _group-by)
                                   :else
                                     series-lab)
                                 (:auto-sort opts true))
           dataset (XYSeriesCollection.)
           chart (do
                  (dorun
                   (map (fn [x y]
                        (if (and (not (nil? x))
                                 (not (nil? y)))
                          (.add data-series (double x) (double y))))
                        __x __y))
                  (.addSeries dataset data-series)
                  (create-plot
                   title
                   x-lab
                   y-lab
                   dataset
                   legend?
                   true  ; tooltips
                   false))
           _ (when x-groups
                (doseq [i (range 1 (count x-groups))]
                  (add-lines chart (nth x-groups i)
                             (nth y-groups i)
                             :series-label (cond
                                             _group-by
                                               (nth (reduce #(if (< (.indexOf %1 %2) 0) (conj %1 %2) %1) [] _group-by) i)
                                             series-lab
                                               series-lab
                                             :else
                                               (format "%s, %s (%s)" 'x 'y i))
                             :points points?)))]
      (.setRenderer (.getPlot chart) 0 (XYLineAndShapeRenderer. true points?))
      (set-theme chart theme)
      chart)))


(defn xy-plot* [x y & options]
  (apply create-xy-series-plot x y create-xy-plot options))

(defmacro xy-plot
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


(defn scatter-plot*
  ([x y & options]
    (let [opts (when options (apply assoc {} options))
          data (or (:data opts) $data)
          _x (data-as-list x data)
          _y (data-as-list y data)
          _group-by (when (:group-by opts)
                      (data-as-list (:group-by opts) data))
          x-groups (when _group-by
                     (map #($ 0 %)
                          (vals ($group-by 1 (conj-cols _x _group-by)))))
          y-groups (when _group-by
                     (map #($ 0 %)
                          (vals ($group-by 1 (conj-cols _y _group-by)))))
          __x (in-coll (if x-groups (first x-groups) _x))
          __y (in-coll (if y-groups (first y-groups) _y))
          title (or (:title opts) "")
          x-lab (or (:x-label opts) (str 'x))
          y-lab (or (:y-label opts) (str 'y))
          series-lab (or (:series-label opts)
                         (if x-groups
                           (format "%s, %s (0)" 'x 'y)
                           (format "%s, %s" 'x 'y)))
          theme (or (:theme opts) :default)
          legend? (true? (:legend opts))
          data-series (XYSeries. series-lab)
          _dataset (XYSeriesCollection.)
          chart (do
                  (dorun
                   (map (fn [x y]
                          (if (and (not (nil? x)) (not (nil? y)))
                            (.add data-series (double x) (double y))))
                        __x __y))
                  (.addSeries _dataset data-series)
                  (org.jfree.chart.ChartFactory/createScatterPlot
                   title
                   x-lab
                   y-lab
                   _dataset
                   org.jfree.chart.plot.PlotOrientation/VERTICAL
                   legend?
                   true            	; tooltips
                   false))
          _ (when x-groups
              (doseq [i (range 1 (count x-groups))]
                (add-points chart
                            (nth x-groups i)
                            (nth y-groups i)
                            :series-label (format "%s, %s (%s)" 'x 'y i))))]
      (.setSeriesShape (-> chart .getPlot .getRenderer) 0 (java.awt.geom.Ellipse2D$Double. -3 -3 6 6))
      (.setSeriesShape (-> chart .getPlot .getRenderer) 1 (java.awt.geom.Rectangle2D$Double. -3 -3 6 6))
      (set-theme chart theme)
      chart)))


(defmacro scatter-plot
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


(defn scatter-plot-matrix*
  [ &{:keys [data group-by title nbins only-first only-triangle]
      :or {data $data
           group-by nil
           title "Scatter Plot Matrix"
           nbins 10 ; number of bars in the histogram
           only-first 6 ; nr of most correlating metrics shown
           only-triangle false }}]
  (let [margin 32
        xmarg 8
        ymarg 16
        group-by-list (if (coll? group-by) group-by [group-by])
        col-names (remove (set group-by-list) (:column-names data))
        col-count (count col-names)
        data-grouped (if (nil? group-by) {:x data} ($group-by group-by-list data))
        rectangles (apply merge (for [xn col-names yn col-names]  {[xn yn] (Rectangle.)}  ))
        xyplot (doto (XYPlot.)
                 (.setRenderer (doto (XYLineAndShapeRenderer. false true)
                                 (.setDrawOutlines true)
                                 (.setBaseFillPaint (Color. 0 0 0 0))
                                 (.setUseFillPaint true)
                                 (.setSeriesPaint 0 (Color/BLUE))
                                 (.setSeriesPaint 1 (Color/RED))
                                 (.setSeriesPaint 2 (Color/GREEN))
                                 (->>
                                  (vector)
                                  (apply (fn [x]
                                           (dotimes [i col-count]
                                             (let [c (.lookupSeriesPaint x i)
                                                   c2 (Color. (.getRed c) (.getGreen c) (.getBlue c) 48 )]
                                               (.setSeriesFillPaint x i c2))))))))
                 (.setRangeGridlinesVisible false)
                 (.setDomainGridlinesVisible false))
        histoplot (doto (XYPlot.)
                    (.setRenderer 1 (doto (XYBarRenderer.)
                                      (.setShadowVisible false)
                                      (.setSeriesPaint 0 (Color. 210 210 210))
                                      (.setBarPainter (StandardXYBarPainter.))))
                    (.setRenderer 0 (doto (XYSplineRenderer.)
                                      (.setShapesVisible false)
                                      (.setSeriesPaint 0 (Color. 170 170 170))
                                      (.setSeriesStroke 0 (BasicStroke. 3))))
                    (.setRangeGridlinesVisible false) ;; these lines do not fit to other range lines
                    (.setDomainGridlinesVisible false)) ; plots for the diagonal
        dataset-impl (fn [x-name y-name] (proxy [AbstractXYDataset] []
                                          ( getDomainOrder [] (DomainOrder/ASCENDING))
                                          ( getXValue [series item] (sel (nth (vals data-grouped) series) :rows item :cols x-name))
                                          ( getYValue [series item] (sel (nth (vals data-grouped) series) :rows item :cols y-name))
                                          ( getItemCount [series] (core/nrow (nth (vals data-grouped) series)))
                                          ( getSeriesKey [series] (str (nth (keys data-grouped) series)))
                                          ( getSeriesCount [] (count data-grouped))))
        histogram-dataset-impl (fn [name]
                                 (doto (HistogramDataset.)
                                   (.addSeries (str name) (double-array ($ name data)) (int nbins))))
        color-for (fn [k] (-> xyplot .getRenderer (.lookupSeriesPaint k)))
        shape-for (fn [k] (-> xyplot .getRenderer (.lookupLegendShape k)))
        font-normal (.getBaseItemLabelFont (.getRenderer xyplot))
        font-bold (.deriveFont font-normal (Font/BOLD))
        legend (let [coll (LegendItemCollection.)]
                 (do
                   (doseq [[k v] (map-indexed vector (keys data-grouped))]
                     (.add coll (doto (LegendItem.
                                       (cond
                                        (map? v) (str (first (nfirst v)))
                                        :else (str v))
                                       "" "" ""
                                       (shape-for k)
                                       (color-for k))))))
                 (identity coll))
        draw-string-left (fn [g2 str x y]
                           (do
                             (let [metr (.getFontMetrics g2)
                                   w    (.stringWidth metr str)
                                   h    (.getHeight metr )]
                               (doto g2
                                 (.setPaint (Color. 255 255 255 128))
                                 (.fillRect x (- y (* h 0.75)) w h)
                                 (.setPaint (Color. 0 0 0))
                                 (.drawString str x y)))))
        draw-string-centered (fn [g2 str x y]
                               (let [metr (.getFontMetrics g2)
                                     w (.stringWidth metr str)
                                     h (.getHeight metr)
                                     xx (int (- x (/ w 2)))
                                     yy (int (+ y (/ h 2)))]
                                 (draw-string-left g2 str xx yy)))
        correlations  (memoize (fn [xn yn] (get (apply merge (for [x col-names y col-names]
                                                              { [x y] (correlation (sel data :cols x) (sel data :cols y)) }))
                                               (sort [xn yn]))))
        variances  (fn [xn] (get (apply merge (for [x col-names] {x (variance (sel data :cols x) ) })) xn ))
        col-names-ordered (take only-first (sort-by (fn [x] (- 0 (reduce + (map (fn [y] (abs (correlations x y))) col-names))))
                                                    col-names))
        key-matrix-all (identity (for [ [yk yv] (map-indexed vector col-names-ordered)
                                        [xk xv] (map-indexed vector col-names-ordered) ]
                                   (vector xk xv yk yv) ))
        key-matrix (if only-triangle (filter (fn [[a b c d]] (>= a c)) key-matrix-all) key-matrix-all)]
    (doto
      (JFreeChart.
        (proxy [Plot] []
          (getLegendItems [] (if (nil? group-by) nil legend))
          (getPlotType [] "Scatter-Plot-Matrix")
          (draw  [g2 area anchor parentState info]
            (let [rect (.createInsetRectangle (.getInsets this) area)
                  axis-space (AxisSpace.)
                  w  (/ (- (.getWidth rect) (* 2 margin)) col-count)
                  h  (/ (- (.getHeight rect) (* 2 margin)) col-count)]
              (do
                (.drawBackground this g2 rect)
                (.drawOutline this g2 rect)
                (doto axis-space (.setLeft 0) (.setTop 0) (.setBottom 0) (.setRight 0))
                (doseq [x [xyplot histoplot]]
                  (doto x
                    (.setInsets    (RectangleInsets. 1 1 1 1))
                    (.setDomainAxis (doto (NumberAxis. " ") (.setAutoRange true) (.setAutoRangeIncludesZero false)))
                    (.setRangeAxis  (doto (NumberAxis. "  ") (.setAutoRange true) (.setAutoRangeIncludesZero false)))
                    (.setFixedDomainAxisSpace axis-space)
                    (.setFixedRangeAxisSpace  axis-space)))
                (dorun (map
                         (fn [ [ x-ind x-name y-ind y-name]]
                           (let [x (+ margin (* w x-ind) (.getX rect))
                                 y (+ margin (* h y-ind) (.getY rect))
                                 rect (doto (get rectangles [x-name y-name]) (.setBounds x y w h))
                                 plot (cond
                                       (== x-ind y-ind) (doto histoplot
                                                          (.setDataset 1 (histogram-dataset-impl x-name))
                                                          (.setDataset 0 (histogram-dataset-impl x-name)))
                                       :else (doto xyplot
                                               (.setDataset (dataset-impl x-name y-name))))]
                             (do
                               (cond
                                (== y-ind 0) (do
                                               (.setTickLabelsVisible (.getDomainAxis plot) (or (odd? x-ind) only-triangle))
                                               (.setDomainAxisLocation plot (AxisLocation/TOP_OR_LEFT))
                                               (.setTickMarksVisible (.getDomainAxis plot) true))
                                (== y-ind (- col-count 1)) (do
                                                             (.setTickLabelsVisible (.getDomainAxis plot) (even? x-ind))
                                                             (.setDomainAxisLocation plot (AxisLocation/BOTTOM_OR_RIGHT))
                                                             (.setTickMarksVisible (.getDomainAxis plot) true))
                                :else (do
                                        (.setTickLabelsVisible (.getDomainAxis plot) false)
                                        (.setTickMarksVisible (.getDomainAxis plot) false)))
                               (cond
                                (== x-ind 0) (do
                                               (.setTickLabelsVisible (.getRangeAxis plot) (odd? y-ind))
                                               (.setRangeAxisLocation plot (AxisLocation/TOP_OR_LEFT))
                                               (.setTickMarksVisible (.getRangeAxis plot) true))
                                (== x-ind (- col-count 1)) (do
                                                             (.setTickLabelsVisible (.getRangeAxis plot) (or (even? y-ind) only-triangle))
                                                             (.setRangeAxisLocation plot (AxisLocation/BOTTOM_OR_RIGHT))
                                                             (.setTickMarksVisible (.getRangeAxis plot) true))
                                :else (do
                                        (.setTickLabelsVisible (.getRangeAxis plot) false)
                                        (.setTickMarksVisible (.getRangeAxis plot) false)))
                                        ; we do have to handle the bottom right element - in case it has axes displayed.
                               (if (and (== x-ind y-ind (- col-count 1)))
                                 (do
                                   (.setVisible (.getRangeAxis histoplot) false)
                                   (.setDataset xyplot (dataset-impl x-name y-name))
                                   (.setTickLabelsVisible (.getRangeAxis xyplot) (odd? col-count))
                                   (.setRangeAxisLocation xyplot (AxisLocation/BOTTOM_OR_RIGHT))
                                   (.setTickMarksVisible (.getRangeAxis xyplot) true)
                                   (.setVisible (.getRangeAxis xyplot) true)
                                   (.draw (.getRangeAxis xyplot) g2 (- (.getMaxX rect) 1) rect rect RectangleEdge/RIGHT info)))
                               (identity (.draw plot g2 rect anchor parentState info))
                               (if (== x-ind y-ind)
                                 (let [str-name (str x-name)
                                       str-var (format "var %.3f\n" (variances x-name ))]
                                   (doto g2
                                     (.setPaint (Color/BLACK))
                                     (.setFont font-normal)
                                     (draw-string-left str-var (int (+ x xmarg)) (int (+ y ymarg)))
                                     (.setFont font-bold)
                                     (draw-string-centered str-name (int (+ x (/ w 2))) (int (+ y (/ h 2))))))
                                 (let [str-cor (format "corr %.3f" (correlations x-name y-name ))]
                                   (doto g2
                                     (.setPaint (Color/BLACK))
                                     (.setFont font-normal)
                                     (draw-string-left str-cor (int (+ x xmarg)) (int (+ y ymarg)))))
                                 ))))
                         key-matrix)))))))
      (.setTitle title)
      (.addSubtitle (doto
                      (TextTitle. (str group-by))
                      (.setPosition (RectangleEdge/BOTTOM)))))))

(defn scatter-plot-matrix
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


(defn function-plot*
  ([function min-range max-range & options]
    (let [opts (when options (apply assoc {} options))
          step-size (or (:step-size opts) (float (/ (- max-range min-range) 500)))
          _x (range-inclusive min-range max-range step-size)
          title (or (:title opts) "")
          x-lab (or (:x-label opts) (format "%s < x < %s" min-range max-range))
          y-lab (or (:y-label opts) (str 'function))
          series-lab (or (:series-label opts) (format "%s" 'function))
          theme (or (:theme opts) :default)
          legend? (true? (:legend opts))]
      (set-theme (xy-plot _x (map function _x)
                          :x-label x-lab
                          :y-label y-lab
                          :title title
                          :series-label series-lab
                          :legend legend?) theme))))


(defmacro function-plot
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


(defn parametric-plot*
  ([function min-range max-range & options]
   (let [opts (when options (apply assoc {} options))
         step-size (or (:step-size opts) (float (/ (- max-range min-range) 500)))
         _t (range-inclusive min-range max-range step-size)
         [_x _y] (apply map vector (map function _t))
         title (or (:title opts) "")
         x-lab (or (:x-label opts) (format "%s < x < %s" (apply min _x) (apply max _x)))
         y-lab (or (:y-label opts) (format "%s < y < %s" (apply min _y) (apply max _y)))
         series-lab (or (:series-label opts) (format "%s" 'function))
         theme (or (:theme opts) :default)
         legend? (true? (:legend opts))]
      (set-theme (xy-plot _x _y
                          :x-label x-lab
                          :y-label y-lab
                          :title title
                          :series-label series-lab
                          :legend legend?
                          :auto-sort false) theme))))


(defmacro parametric-plot
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


;;problem with heat-map is that we miss the gridlines.
;;strategy is to overlay them on top of the jfreechart xyplot by
;;using a proxy for xyplot that overrides its final rendering pass
;;drawAnnotations.  drawAnnotations will invoke drawDomainGridLines
;;etc. prior to invoking super's drawAnnotations.
(defn ^XYPlot ->grided-xy-plot
  "Creates an xyplot that ensures gridlines are drawn over he plot
   content.  Better way is to define render order via layers,
   but we'll work within JFree's constraints and avoid
   redefining the entire draw method."
  [dataset xaxis yaxis renderer]
  (let [axis-state   (atom nil)
        parent-state (atom nil)]
    (proxy [XYPlot] [dataset xaxis yaxis renderer]
      ;;on draw, we save info we may need later
      (draw [g2  area anchor  parentState info]
        (do (reset! parent-state parentState)
            (proxy-super draw g2 area anchor parentState info)))
      ;;save intermediate axis-state results
      (drawAxes [g2 plot-area data-area plot-state]
        (let [state-map (proxy-super drawAxes g2 plot-area data-area plot-state)
              _         (reset! axis-state state-map)]
          state-map))
      ;;for the final pass, to overlay gridlines onto the
      ;;heat-map, prior to annotations, we render the gridlines
      ;;using stored information (they've already been rendered,
      ;;but can't be seen).
      (drawAnnotations [g2 data-area info]
        (let [state       @axis-state
              parent      @parent-state
              domain-axis (.getDomainAxis this)
              range-axis  (.getRangeAxis  this)
              domainAxisState (or (get state domain-axis)
                                  (get parent domain-axis))
              rangeAxisState (or (get state range-axis)
                                 (get parent range-axis))
              ]
          (do (proxy-super drawDomainGridlines g2 data-area (.getTicks domainAxisState))
              (proxy-super drawRangeGridlines g2 data-area  (.getTicks rangeAxisState))
              (proxy-super drawAnnotations g2 data-area info)))))))

;;Pending....need to fill in chart operations from data.clj
(defn stacked-areaxy-chart
  [^xydataset xytable & options]
  (let [opts         (if options (apply assoc {:legend true :aa false} options)
                         {:legend true  :aa false})
        ;; _values     (data-as-list values data) ;old
        ;; _categories (data-as-list categories data) ;;only difference
        ;;is that categories are now series...
        title        (or   (:title opts)        "")
        theme        (or   (:theme opts)  :default)
        ;;new 
        _color-by    (or (:color-by opts)
                         #_(get *trend-info* :color))
        ;;group-by is now the series....
        x-label      (or (:x-label opts) "time (days)")
        y-label      (or (:y-label opts) "quantity required (units)")
        series-label (:series-label opts)
        vertical?    (if (false? (:vertical opts)) false true)
        legend?      (true? (:legend opts))
        _order-by    (or (:order-by opts) #_(get *trend-info* :order))
        tickwidth    (when  (:tickwidth opts) (:tickwidth opts))
        ^xydataset 
        xytable      (order-series-by xytable _order-by)
        chart        (org.jfree.chart.ChartFactory/createStackedXYAreaChart
                      title
                      x-label
                      y-label
                      (.table xytable)
                      (if vertical?
                        org.jfree.chart.plot.PlotOrientation/VERTICAL
                        org.jfree.chart.plot.PlotOrientation/HORIZONTAL)
                      legend?
                      true
                      false)]
    ;;the difference between the regular area and this guy is that
    ;;we have a category, defined by group-by, and the xs and ys....
    ;;I'll rename _categories to xs at some point and values to ys......
    (let [num-axis (.getRangeAxis (.getPlot chart))
          num-form (java.text.NumberFormat/getNumberInstance)]
      (do
        (.setMaximumFractionDigits num-form 0)
        (.setNumberFormatOverride num-axis num-form)
                
        (.setAntiAlias chart  (get opts :aa))
        (set-colors    chart _color-by)
        (when tickwidth (set-xticks chart tickwidth))
        (set-theme     chart  theme)
        chart))))

;;okay....this shitshow is going to be wrapped soon
(defn ->stacked-area-chart [ds & {:keys [row-field col-field values-field order-by]
                                  :or {row-field :start
                                       col-field :category
                                       values-field :quantity
                                       } :as opts}]
  (let [xyt  (xy-table row-field values-field :group-by col-field :data ds)]    
    (as-chart xyt (assoc opts :order-by (or order-by (fn [x] x))))))
