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

(defn get-value   [^CategoryDataset cd s x]
  (if (number? s)
    (.getValue cd (int s) (int x))
    (.getValue cd ^Comparable s ^Comparable x)))

;;need to add hints, but we overload this...
;;There's also the variant where we add multiple values.
(defn add-value [^DefaultCategoryDataset cd
                 ^Comparable series
                 ^Comparable category
                 ^Number v]
  (doto cd (.addValue v series category)))

(defn category-values [^CategoryDataset cd]
  (for [series (row-keys cd)
        col    (col-keys cd)]
    {:series   series
     :category col
     :value    (get-value cd series col)}))

(defn values->categorydataset [vs]
  (reduce (fn [^DefaultCategoryDataset acc {:keys [series category value]
                                            :or   {series "blank"}}]
            (add-value acc series category value))
          (DefaultCategoryDataset.) vs))

(defn series-key [^Series s] (.getSeriesKey s))

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



