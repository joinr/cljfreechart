(ns cljfreechart.charts.frequencies)

(defn add-histogram*
  ([chart x & options]
    (let [opts (when options (apply assoc {} options))
          data (or (:data opts) $data)
          _x (data-as-list x data)
          data-plot (.getPlot chart)
          n (.getDatasetCount data-plot)
          nbins (or (:nbins opts) 10)
          series-lab (or (:series-label opts) (str 'x))]
      (do
        (.addSeries (.getDataset data-plot) series-lab (double-array _x) nbins)
        (.setSeriesOutlinePaint (-> chart .getPlot .getRenderer) n java.awt.Color/lightGray)
        (.setSeriesRenderingOrder data-plot org.jfree.chart.plot.SeriesRenderingOrder/FORWARD)
        (.fireChartChanged chart)
        chart))))

(defmacro add-histogram
  "
  Adds a histogram to an existing histogram plot, returns the modified
  chart object.

  Options:
    :nbins (default 10) number of bins for histogram
    :series-label (default x expression)

  Examples:

    (use '(incanter core charts stats datasets))
    (doto (histogram (sample-normal 1000)
                     :legend true)
          view
          (add-histogram (sample-normal 1000 :sd 0.5)))


    (with-data (get-dataset :iris)
      (doto (histogram :Sepal.Length :legend true)
        (add-histogram :Petal.Length)
        view))

  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([chart x & options]
    `(let [opts# ~(when options (apply assoc {} options))
           series-lab# (or (:series-label opts#) (str '~x))
           args# (concat [~chart ~x]
                         (apply concat (seq (apply assoc opts#
                                                   [:series-label series-lab#]))))]
        (apply add-histogram* args#))))


(defn histogram*
  ([x & options]
    (let [opts (if options (apply assoc {} options) {})
          data (or (:data opts) $data)
          _x (data-as-list x data)
          nbins (or (:nbins opts) 10)
          theme (or (:theme opts) :default)
          density? (true? (:density opts))
          title (or (:title opts) "")
          x-lab (or (:x-label opts) (str 'x))
          y-lab (or (:y-label opts)
                     (if density? "Density" "Frequency"))
          series-lab (or (:series-label opts) (str 'x))
          legend? (true? (:legend opts))
          dataset (HistogramDataset.)]
      (do
        (.addSeries dataset series-lab (double-array _x) nbins)
        (when density? (.setType dataset org.jfree.data.statistics.HistogramType/SCALE_AREA_TO_1))
        (let [chart (-> (org.jfree.chart.ChartFactory/createHistogram
                          title
                          x-lab
                          y-lab
                          dataset
                          org.jfree.chart.plot.PlotOrientation/VERTICAL
                          legend?		; no legend
                          true			; tooltips
                          false)
                        (set-theme theme))]
          chart)))))


(defmacro histogram
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
