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
            [org.jfree.data.gannt
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
             CategoryToPieDataset
             DefaultCategoryDataset
             DefaultIntervalCategoryDataset
             SlidingCategoryDataset]
            [java.awt Color]))

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



