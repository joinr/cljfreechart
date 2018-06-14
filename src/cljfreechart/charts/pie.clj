(ns cljfreechart.charts.pie)

(defn pie-chart*
  ([categories values & options]
    (let [opts (when options (apply assoc {} options))
          data (or (:data opts) $data)
          _values (data-as-list values data)
          _categories (data-as-list categories data)
          title (or (:title opts) "")
          theme (or (:theme opts) :default)
          legend? (true? (:legend opts))
          dataset (DefaultPieDataset.)
          chart (org.jfree.chart.ChartFactory/createPieChart
                  title
                  dataset
                  legend?
                  true
                  false)]
      (do
        (doseq [i (range 0 (count _values))]
          (.setValue dataset (nth _categories i) (nth _values i)))
        (set-theme chart theme)
        chart))))

(defmacro pie-chart
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
