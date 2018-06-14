(ns cljfreechart.charts.timeseries)

(defn- create-time-series-plot
  [title x-lab y-lab dataset legend? tooltips? urls?]
  (org.jfree.chart.ChartFactory/createTimeSeriesChart
    title
    x-lab
    y-lab
    dataset
    legend?
    tooltips?
    urls?))

(defn time-series-plot* [x y & options]
  (apply create-xy-series-plot x y create-time-series-plot options))

(defmacro time-series-plot
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
