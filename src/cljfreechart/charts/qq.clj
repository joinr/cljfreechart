(ns cljfreechart.charts.qq)
(defn qq-plot
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
