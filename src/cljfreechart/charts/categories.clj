(ns cljfreechart.charts.categories)

(defn add-categories*
  ([chart categories values & options]
    (let [opts (when options (apply assoc {} options))
          data (or (:data opts) $data)
          _values (data-as-list values data)
          _categories (data-as-list categories data)
          _group-by (when (:group-by opts)
                      (data-as-list (:group-by opts) data))
           _chart chart
           series-label (:series-label opts)
           data-plot (.getCategoryPlot _chart)
           n-col (.getColumnCount (.getDataset data-plot))
           n-row (.getRowCount (.getDataset data-plot))]
        (do
          (doseq [i (range 0 (count _values))] (.addValue (.getDataset data-plot)
                                                      (nth _values i)
                                                      (cond
                                                       _group-by
                                                         (nth _group-by i)
                                                       series-label
                                                         series-label
                                                       :else
                                                         (str 'values))
                                                       (nth _categories i)))
          chart))))


(defmacro add-categories
  "
  Adds an additional categories to an existing bar-chart or line-chart, returns the modified chart object.

  Options:
    :group-by
    :series-label

  Examples:

    (use '(incanter core charts stats datasets))
    (def seasons (mapcat identity (repeat 3 [\"winter\" \"spring\" \"summer\" \"fall\"])))
    (def years (mapcat identity (repeat 4 [2007 2008 2009])))
    (def x (sample-uniform 12 :integers true :max 100))
    (def plot (bar-chart years x :group-by seasons :legend true))
    (view plot)
    (add-categories plot years [10 20 40] :series-label \"winter-break\")

    (add-categories plot
                    (plus 3 years)
                    (sample-uniform 12 :integers true :max 100)
                    :group-by seasons)

    (def plot2 (line-chart years x :group-by seasons :legend true))
      (view plot2)
      (add-categories plot2 (plus 3 years) (sample-uniform 12 :integers true :max 100) :group-by seasons)

      (with-data (get-dataset :iris)
        (doto (line-chart :Species :Sepal.Length
                          :data ($rollup mean :Sepal.Length :Species)
                          :legend true)
          (add-categories :Species :Sepal.Width :data ($rollup mean :Sepal.Width :Species))
          (add-categories :Species :Petal.Length :data ($rollup mean :Petal.Length :Species))
          (add-categories :Species :Petal.Width :data ($rollup mean :Petal.Width :Species))
          view))


  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([chart categories values & options]
    `(let [opts# ~(if options (apply assoc {} options) {})
           group-by# (:group-by opts#)
           series-lab# (or (:series-label opts#)
                           (if group-by#
                             (format "%s, %s (0)" '~categories '~values)
                             (format "%s, %s" '~categories '~values)))
           args# (concat [~chart ~categories ~values]
                         (apply concat (seq (apply assoc opts# [:series-label series-lab#]))))]
       (apply add-categories* args#))))


(defn line-chart*
  ([categories values & options]
    (let [opts (when options (apply assoc {} options))
          data (or (:data opts) $data)
          _values (data-as-list values data)
          _categories (data-as-list categories data)
          title (or (:title opts) "")
          group-by (when (:group-by opts)
                     (data-as-list (:group-by opts) data))
          x-label (or (:x-label opts) (str 'categories))
          y-label (or (:y-label opts) (str 'values))
          series-label (:series-label opts)
          vertical? (if (false? (:vertical opts)) false true)
          theme (or (:theme opts) :default)
          legend? (true? (:legend opts))
          dataset (DefaultCategoryDataset.)
          chart (org.jfree.chart.ChartFactory/createLineChart
                 title
                 x-label
                 y-label
                 dataset
                 (if vertical?
                   org.jfree.chart.plot.PlotOrientation/VERTICAL
                   org.jfree.chart.plot.PlotOrientation/HORIZONTAL)
                 legend?
                 true
                 false)]
      (do
        (doseq [i (range 0 (count _values))] (.addValue dataset
                                                       (nth _values i)
                                                       (cond
                                                        group-by
                                                          (nth group-by i)
                                                        series-label
                                                          series-label
                                                        :else
                                                          (str 'values))
                                                       (nth _categories i)))
        (set-theme chart theme)
        chart))))


(defmacro line-chart
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


(defn bar-chart*
  ([categories values & options]
    (let [opts (when options (apply assoc {} options))
          data (or (:data opts) $data)
          _values (data-as-list values data)
          _categories (data-as-list categories data)
          title (or (:title opts) "")
          theme (or (:theme opts) :default)
          _group-by (when (:group-by opts)
                      (data-as-list (:group-by opts) data))
          x-label (or (:x-label opts) (str 'categories))
          y-label (or (:y-label opts) (str 'values))
          series-label (:series-label opts)
          vertical? (if (false? (:vertical opts)) false true)
          legend? (true? (:legend opts))
          dataset (DefaultCategoryDataset.)
          chart (org.jfree.chart.ChartFactory/createBarChart
                  title
                  x-label
                  y-label
                  dataset
                  (if vertical?
                    org.jfree.chart.plot.PlotOrientation/VERTICAL
                    org.jfree.chart.plot.PlotOrientation/HORIZONTAL)
                   legend?
                   true
                   false)]
      (do
        (doseq [i (range 0 (count _values))]
          (.addValue dataset
                     (nth _values i)
                     (cond
                       _group-by
                         (nth _group-by i)
                       series-label
                         series-label
                       :else
                         (str 'values))
                     (nth _categories i)))
        (set-theme chart theme)
        chart))))

(defmacro bar-chart
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


(defn area-chart*
  ([categories values & options]
     (let [opts (when options (apply assoc {} options))
           data (or (:data opts) $data)
          _values (data-as-list values data)
          _categories (data-as-list categories data)
           title (or (:title opts) "")
           theme (or (:theme opts) :default)
           _group-by (when (:group-by opts)
                       (data-as-list (:group-by opts) data))
           x-label (or (:x-label opts) (str 'categories))
           y-label (or (:y-label opts) (str 'values))
           series-label (:series-label opts)
           vertical? (if (false? (:vertical opts)) false true)
           legend? (true? (:legend opts))
           dataset (DefaultCategoryDataset.)
           chart (org.jfree.chart.ChartFactory/createAreaChart
                     title
                     x-label
                     y-label
                     dataset
                     (if vertical?
                       org.jfree.chart.plot.PlotOrientation/VERTICAL
                       org.jfree.chart.plot.PlotOrientation/HORIZONTAL)
                     legend?
                     true
                     false)]
        (do
          (doseq [i (range 0 (count _values))]
            (.addValue dataset
                       (nth _values i)
                       (cond
                        _group-by
                          (nth _group-by i)
                        series-label
                          series-label
                        :else
                          (str 'values))
                       (nth _categories i)))
          (set-theme chart theme)
          chart))))


(defmacro area-chart
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


(defn stacked-area-chart*
  ([categories values & options]
    (let [opts (when options (apply assoc {} options))
          data (or (:data opts) $data)
          _values (data-as-list values data)
          _categories (data-as-list categories data)
          title (or (:title opts) "")
          theme (or (:theme opts) :default)
          _group-by (when (:group-by opts)
                      (data-as-list (:group-by opts) data))
          x-label (or (:x-label opts) (str 'categories))
          y-label (or (:y-label opts) (str 'values))
          series-label (:series-label opts)
          vertical? (if (false? (:vertical opts)) false true)
          legend? (true? (:legend opts))
          dataset (DefaultCategoryDataset.)
          chart (org.jfree.chart.ChartFactory/createStackedAreaChart
                  title
                  x-label
                  y-label
                  dataset
                  (if vertical?
                    org.jfree.chart.plot.PlotOrientation/VERTICAL
                    org.jfree.chart.plot.PlotOrientation/HORIZONTAL)
                    legend?
                    true
                    false)]
      (do
        (doseq [i (range 0 (count _values))]
          (.addValue dataset
                     (nth _values i)
                     (cond
                      _group-by
                        (nth _group-by i)
                      series-label
                        series-label
                      :else
                        (str 'values))
                     (nth _categories i)))
          (set-theme chart theme)
          chart))))

(defmacro stacked-area-chart
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

(defn stacked-bar-chart*
  ([categories values & options]
     (let [opts (when options (apply assoc {} options))
           data (or (:data opts) $data)
          _values (data-as-list values data)
          _categories (data-as-list categories data)
           title (or (:title opts) "")
           theme (or (:theme opts) :default)
           _group-by (when (:group-by opts)
                       (data-as-list (:group-by opts) data))
           x-label (or (:x-label opts) (str 'categories))
           y-label (or (:y-label opts) (str 'values))
           series-label (:series-label opts)
           vertical? (if (false? (:vertical opts)) false true)
           legend? (true? (:legend opts))
           dataset (DefaultCategoryDataset.)
           chart (org.jfree.chart.ChartFactory/createStackedBarChart
                     title
                     x-label
                     y-label
                     dataset
                     (if vertical?
                       org.jfree.chart.plot.PlotOrientation/VERTICAL
                       org.jfree.chart.plot.PlotOrientation/HORIZONTAL)
                     legend?
                     true
                     false)]
        (do
          (doseq [i (range 0 (count _values))]
            (.addValue dataset
                       (nth _values i)
                       (cond
                        _group-by
                          (nth _group-by i)
                        series-label
                          series-label
                        :else
                          (str 'values))
                       (nth _categories i)))
          (set-theme chart theme)
          chart))))


(defmacro stacked-bar-chart
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
