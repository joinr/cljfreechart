;;wrappers and operations on
;;jfreechart datasets.
(ns cljfreechart.data
  (:import  [org.jfree.data.xy
             DefaultHighLowDataset
             DefaultIntervalXYDataset
             DefaultOHLCDataset    ;;high/low plot
             DefaultWindDataset    ;;x,y,direction,mag plots
             DefaultXYDataset      ;;various xy plots
             DefaultXYZDataset     ;;heatmaps
             DefaultTableXYDataset ;;stacked
             XYSeries
             XYSeriesCollection
             XYDataItem]
            [org.jfree.data.gantt
             XYTaskDataset
             SlidingGanttCategoryDataset
             TaskSeriesCollection
             TaskSeries
             Task]
            [org.jfree.data.general
             AbstractDataset
             DefaultHeatMapDataset
             DefaultKeyedValueDataset
             DefaultKeyedValues2DDataset
             DefaultKeyedValuesDataset
             DefaultPieDataset
             DefaultValueDataset
             Series
             WaferMapDataset]
            ;;category datasets...
            [org.jfree.data.category
             CategoryDataset
             CategoryToPieDataset
             DefaultCategoryDataset
             DefaultIntervalCategoryDataset
             SlidingCategoryDataset]
            [org.jfree.chart.plot Plot]
            [java.awt Color]))

;;datasets are composed of series.
;;series come in some flavors:

;;two most common types of datasets:
;;xyseriescollection
;;  getSeriesCount and friends, composed of
;;  multiple XYSeries...

;;defaultcategorydataset
;;acts like a table structure.
;;Has Rows and Columns

;;for categorical data, we get this...

;;series keys, probably better abstraction
(defn row-keys [^CategoryDataset cd] (.getRowKeys cd))
;; domain keys (categories) ~x axis
(defn col-keys [^CategoryDataset cd] (.getColumnKeys cd))

;;the problem we have is that DefaultCategoryDataset
;;doesn't maintain series objects.

(defprotocol ISeriesColl
  (add-value
    [obj series x y]
    [obj record])
  (get-value
    [obj series x]
    [obj record])
  (series-keys [obj]))

(extend-protocol ISeriesColl
  DefaultCategoryDataset
  (add-value
    ([cd ^Comparable  series  ^Comparable category ^Number v]
     (doto cd (.addValue v series category)))
    ([cd record]
     (let [{:keys [series category value]
            :or   {series "blank"}} record]
       (doto cd (.addValue ^Number value
                           ^Comparable series ^Comparable category)))))
  (get-value
    ([cd series category]
     (if (number? series)
       (.getValue cd (int series) (int category))
       (.getValue cd ^Comparable series ^Comparable category)))
    ([cd record]
     (let [{:keys [series category]} record]
       (if (number? series)
         (.getValue cd (int series) (int category))
         (.getValue cd ^Comparable series ^Comparable category)))))
  (series-keys [cd] (row-keys cd)))

(defn category-values [^CategoryDataset cd]
  (for [series (row-keys cd)
        col    (col-keys cd)]
    {:series   series
     :category col
     :value    (get-value cd series col)}))

(defn values->categorydataset [vs]
  (reduce add-value (DefaultCategoryDataset.) vs))

(defn series-key [^Series s] (.getSeriesKey s))

(defn get-series [])

(extend-protocol ISeriesColl
  XYSeriesCollection 
  (add-value
    ([cd ^Comparable  series  ^Comparable x ^Number v]
     (doto cd (.addValue v series x)))
    ([cd record]
     (let [{:keys [series category value]
            :or   {series "blank"}} record]
       (doto cd (.addValue ^Number value
                           ^Comparable series ^Comparable category)))))
  (get-value
    ([cd series category]
     (if (number? series)
       (.getValue cd (int series) (int category))
       (.getValue cd ^Comparable series ^Comparable category)))
    ([cd record]
     (let [{:keys [series category]} record]
       (if (number? series)
         (.getValue cd (int series) (int category))
         (.getValue cd ^Comparable series ^Comparable category)))))
  (series-keys [cd] (row-keys cd)))

;;xy-series
(defn xy-series [^XYSeriesCollection xys]
  (let [cnt (.getSeriesCount xys)]
    (for [i (range cnt)]
      [(.getSeriesKey xys (int i))
       (.getSeries xys (int i))])))

(defn xy-values [^XYSeriesCollection xys]
  (for [[k series] (xy-series xys)
        item      (.getItems series)]
    {:series k  :x (.getX item) :y (.getY item)}))

;;possibly need to autosort
(defn values->xydataset [vs]
  (let [series (atom {})
        get-series! (fn [^Comparable k]
                      (or (@series k)
                          (let [s (XYSeries. k)
                                _ (swap! series assoc k s)]
                            s)))]
    (doseq [{:keys [series x y]
             :or {series "blank"}} vs]
      (doto ^XYSeries (get-series! series)
        (.add (double x) (double y))))
    (reduce (fn [^XYSeriesCollection acc ^XYSeries s]
              (doto acc (.addSeries s)))
            (XYSeriesCollection.)
            (vals @series))))

;;return a sequence of the dataset and associated
;;information, position, renderer
(defn datasets [^org.jfree.chart.plot.Plot plt]
  (let [n (.getDatasetCount plt)]
    (for [i (range n)]
      {:dataset-idx i
       :data (.getDataset plt i)
       :renderer (.getRenderer plt i)})))

(defprotocol IPlotContainer
  (as-plot [obj]))

(extend-protocol IPlotContainer
  org.jfree.chart.plot.Plot
  (as-plot [obj] obj)
  org.jfree.chart.JFreeChart
  (as-plot [obj] (.getPlot obj)))

(defprotocol IDataValues
  (-records [obj]))

(extend-protocol IDataValues
  XYSeriesCollection
  (-records [obj] (xy-values obj))
  DefaultCategoryDataset
  (-records [obj] (category-values obj))
  org.jfree.chart.plot.Plot
  (-records [obj]
    (->> (datasets obj)
         (mapcat (fn [{:keys [dataset-idx data]}]
                   (map (fn [r]
                          (assoc r :dataset-idx dataset-idx))
                        (-records data)))))))

(defn records
  "Gives us a seq-of-maps view over a dataset.  Intended for
   wrapping jfreechart dataset providers."
  [obj]
  (if (extends? IDataValues (type obj))
    (-records obj)
    (-records (as-plot obj))))

;;this is looking similar to vega/gg
;;we don't have aesthetics like size...

;;plot has a bunch of graphics and customization options.
;;maybe we don't care about that, and keep the aesthetics
;;separate?
(defn plot-info [x]
  (let [^Plot plot (as-plot x)]
    {:plot            plot
     :data            (vec (datasets plot))
     :rendering-order (.getDatasetRenderingOrder plot)}))

(defn data-map
  "Given a IPlotContainer x, returns a map of {dataset-idx data}
   to provide quick access and querying for plots.
   Map is currently unordered!"
  [x]
  (->> (for [{:keys [dataset-idx data renderer] :as r} (-> x plot-info :data)]
         [dataset-idx (dissoc r :dataset-idx)])
       (into {})))

(defn data-records
  "Given a IPlotContainer x, returns a map of {dataset-idx [r1 r2]}
  to provide quick access and querying for plots.
  Map is currently unordered!"
  [x]
  (into {}
     (for [[idx {:keys [data]}] (data-map x)]
       [idx (records data)])))

;;ahh...
;;should be able to construct plots from data-maps...
;;make it easy to view as a core.matrix dataset?
;;then we can leverage everything from incanter directly?
(defn data-map->plot []
  )



;;From the clojure perspective,
;;a simple bar-chart with one layer:
;;1) a dataset, build from said url.

;;2) transformed to include only records where year = 2000

;;Rendering:
;;3) the mark for the layer is bar...
;;   Tell the implementation to use an appropriate renderer
;;   for bars.

;; visual channels
;;  For the x-coordinates in this layer,
;;    transform the data by grouping by people,
;;    and summing, returning a quantitative field.
;;    Create a custom axis (for x),
;;        where the title is population.
;;        otherwise we stick to defaults.

(comment
{"data" {"url" "data/population.json"},
 "transform" [{"filter" "datum.year == 2000"}],
 "mark" "bar",
 "encoding" {"x" {"aggregate" "sum", "field" "people", "type" "quantitative",
                  "axis" {"title" "population"}}}}
)

;;This provides a read-only representation.
;;We should allow the API to provide updates
;;or writes.  So mutable part of the API
;;should handle adding series and the like...

;;We'll create a generic styling API
;;to inherit from, then allow implementation
;;specific options.


;;now we can easily read datasets, even after
;;getting them into charts.

;;what about altering them?

;;I think we want to specify constructors for
;;plots...



(comment
  (use '[incanter datasets charts core])
  (def catplt (->> (get-dataset :airline-passengers)
                   (area-chart :year :passengers :group-by :month :legend true
                               :data)))

  (def xyplt   (with-data (get-dataset :iris)
                 (scatter-plot :Sepal.Length :Sepal.Width :group-by :Species)))


  )

;;for xyseries, we get something similar...

;;values

;;we'd like to define coercions.

;;For a lot of these, it looks like we can simply coerce
;;to a double-array or matrix and we're good too go.

;;There seem to be ctors for arrays.


;;most of these operations reify to operations on datasets...
;;the charts listen to dataset change events, and update
;;in reponse.

(comment 
;;we can abstract this further....if we wrap the dataset.
(defn ^CategoryDataset add-categories!
  "Projects two sequences of identical length - values and categories,
   onto a category dataset.  An optional vector of groupings, as if by
   group-by - groups - may be supplied to infer different series labels.
   Defaults to a single series/group called 'values"
  [^CategoryDataset dataset values categories
                      & {:keys [groups series-label]
                         :or {series-label (str 'values)}}]
  (let [values     (vec values)
        categories (vec categories)
        groups     (and (seq goups) (vec groups))
        row->series (if groups
                      (fn [idx] (nth groups idx))
                      (fn [idx] series-label))]
    (doseq [i (range 0 (count values))]
      (.addValue dataset
                 (nth values i)
                 (row->series i)
                 (nth categories i)))
    dataset))

;;we're assuming - VERY SIMPLY - that values, categories, and group-by are
;;sequences of identical length.  Basically, pre-cooked...
(defn ^DefaultCategoryDataset ->category-dataset
  "Projects two sequences of identical length - values and categories,
   onto a new category dataset. An optional vector of groupings, as if by
   group-by - groups - may be supplied to infer different series labels. Defaults
   to a single series/group called 'values.  Returns the resulting dataset  -
   typically for use in plotting."
  [values categories & {:keys [groups series-label]
                          :or {series-label (str 'values)}}]
  (add-categories! (DefaultCategoryDataset.) values categories
                   :groups groups :series-label series-label))

)

