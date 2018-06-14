(ns cljfreechart.charts.box)

(defn box-plot*
  ([x & options]
    (let [opts (when options (apply assoc {} options))
          data (or (:data opts) $data)
          _x (data-as-list x data)
          _group-by (when (:group-by opts)
                      (data-as-list (:group-by opts) data))
          x-groups (when _group-by
                     (->> (conj-cols _x _group-by)
                          ($group-by 1)
                          vals
                          (map #($ 0 %))
                          (map in-coll)))
          __x (if x-groups (first x-groups) _x)
          title (or (:title opts) "")
          x-label (or (:x-label opts) "")
          y-label (or (:y-label opts) (str 'x))
          series-label (or (:series-label opts) (str 'x))
          category-label (or (:category-label opts) 0)
          group-labels (:group-labels opts)
          theme (or (:theme opts) :default)
          legend? (true? (:legend opts))
          dataset (DefaultBoxAndWhiskerCategoryDataset.)
          chart (org.jfree.chart.ChartFactory/createBoxAndWhiskerChart
                 title
                 x-label
                 y-label
                 dataset
                 legend?)]
      (-> chart .getCategoryPlot .getRenderer (.setMaximumBarWidth 0.25))
      (.add dataset __x
            (if _group-by
              (str series-label " (0)")
              series-label)
            category-label)
      (when-not (empty? (rest x-groups))
        (doseq [i (range 1 (count x-groups))]
          (.add dataset
                (nth x-groups i)
                (str series-label " (" i ")") i)))
      (set-theme chart theme)
      chart)))

(defmacro box-plot
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

(defn add-box-plot*
  ([chart x & options]
    (let [opts (when options (apply assoc {} options))
          data (or (:data opts) $data)
          _x (data-as-list x data)
          data-plot (.getCategoryPlot chart)
          n-col (.getColumnCount (.getDataset data-plot))
          n-row (.getRowCount (.getDataset data-plot))
          series-label (or (:series-label opts) (str 'x))
          category-label (or (:category-label opts)
                               (str n-col))]
      (do
        (.add (.getDataset data-plot) _x series-label category-label)
        chart))))

(defmacro add-box-plot
  "
  Adds an additional box to an existing box-plot, returns the modified chart object.

  Options:
    :series-label (default x expression)

  Examples:

    (use '(incanter core charts stats datasets))
    (doto (box-plot (sample-normal 1000) :legend true)
          view
          (add-box-plot (sample-normal 1000 :sd 2))
          (add-box-plot (sample-gamma 1000)))


    (with-data (get-dataset :iris)
      (doto (box-plot :Sepal.Length :legend true)
        (add-box-plot :Petal.Length)
        (add-box-plot :Sepal.Width)
        (add-box-plot :Petal.Width)
        view))


  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([chart x & options]
    `(let [opts# ~(when options (apply assoc {} options))
           series-lab# (or (:series-label opts#) (str '~x))
           args# (concat [~chart ~x] (apply concat (seq (apply assoc opts#
                                                        [:series-label series-lab#]))))]
        (apply add-box-plot* args#))))


