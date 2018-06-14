(ns cljfree.charts.ba)

(defn bland-altman-plot
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
