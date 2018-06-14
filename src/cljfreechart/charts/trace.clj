(ns cljfreechart.charts.trace)
(defn trace-plot
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
                             
