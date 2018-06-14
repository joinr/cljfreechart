;;wrappers and operations on
;;jfreechart datasets.
(ns cljfreechart.data
  (:import  [org.jfree.data.xy DefaultTableXYDataset 
             XYSeries XYSeriesCollection 
             XYDataItem]
            [org.jfree.data.general Series AbstractDataset]
            [java.awt.Color]))

;;Notes:
;;UGH, jfree uses a shitload of classes for things that
;;we could arguably use simple clojure structures for.
;;Like Vector (just a 2d element), VectorDataItem (4 datums).
;;I'd like to rewrite this to be simpler....

;;For now, we WRAP!


;;Design considerations...
;;Chart as plot
;;plot has dataset+
;;plot has renderers+
;;dataset has series+


;;Abstractions for working with charts as data:
;;We have layers.
;;A chart wraps the jfreechart object (or extends
;;a protocol to it to provide access to stuff).

;;We view our chart's plot
;;as a bunch of layers...


;;plot.mapDataSetToRange 

(comment
  
(->xy-plot
 :layers [^:scatter    (->scatter-plot xs ys opts)  
          ^:histogram  (->line-plot xs ys opts)])

;;alternately...
(-> (->xy-plot)  ;;chart with XYPlot, XYItemRenderer
    (add-data      :points xs ys) ;;XYDataSet...
    (add-axis      :x-axis xs)    ;;define x-axis named :x-axis, mapped to points.
    (ass-axis      :y-axis ys)    ;;define y-axis named :y-axis, mapped to points.
    (map-data      :points :x-axis :y-axis)
    (add-renderer  :xy-points :points))

;;xform to a builder-like spec...
(->xy-plot  ;;chart with XYPlot, XYItemRenderer
 {:data        {:points    [xs ys]} ;;XYDataSet...
  :x-axis      {:x-axis ...}   ;;define x-axis named :x-axis, mapped to points.
  :y-axis      {:y-axis ...}     ;;define y-axis named :y-axis, mapped to points.
  :map-data    [:points  :x-axis :y-axis]
  :marks       {:points :xy-points}})

{:plot   :xy-plot
 :data   {:points [xs ys]}
 :x-axis :x-axis
 :y-axis :y-axis
 :marks  {:xy-points :points}
 }

{:plot   :xy-plot
 :data   {:points [xs ys]}
 :x-axis :x-axis
 :y-axis :y-axis
 :group-by f
 :marks  {:xy-points :points}
 }
)

(defn- create-xy-series-plot
  ([x y create-plot & options]
     (let [opts (when options (apply assoc {} options))
           data (or (:data opts) $data) ;;could be nil
           ;;build the dataset, derive series if applicable.
           _x (data-as-list x data)
           _y (data-as-list y data)
           _group-by (when (:group-by opts)
                       (data-as-list (:group-by opts) data))
           ;;organize data series by groups iff _group-by exists
           x-groups (when _group-by
                      (let [x-groupped (->> (conj-cols _x _group-by)
                                            ($group-by 1))]
                        (->> (distinct _group-by)
                             (map #(->>
                                    (get x-groupped {1 %})
                                    ($ 0))))))
           y-groups (when _group-by
                      (let [y-groupped (->> (conj-cols _y _group-by)
                                            ($group-by 1))]
                        (->> (distinct _group-by)
                             (map #(->>
                                    (get y-groupped {1 %})
                                    ($ 0))))))
           ;;organize the data into x and y, injecting into [] if not
           ;;a collection.  This will take the first series and
           ;;return a compatible sequence of series.
           __x (in-coll (if x-groups (first x-groups) _x))
           __y (in-coll (if y-groups (first y-groups) _y))
           
           ;;chart properties
           title (or (:title opts) "")
           x-lab (or (:x-label opts) (str 'x))
           y-lab (or (:y-label opts) (str 'y))
           
           series-lab (or (:series-label opts)
                          (if x-groups
                            (format "%s, %s (0)" 'x 'y)
                            (format "%s, %s" 'x 'y)))
           theme (or (:theme opts) :default)
           legend? (true? (:legend opts))
           points? (true? (:points opts))
           
           ;;define 1 or more series
           data-series (XYSeries. (cond
                                   _group-by
                                     (first _group-by)
                                   :else
                                     series-lab)
                                  (:auto-sort opts true))

           ;;build the dataset
           dataset (XYSeriesCollection.)
           _        (do ;inject the xy coords from the serise into dataset by building xyseries.
                      (dorun
                       (map (fn [x y]
                              (if (and (not (nil? x))
                                       (not (nil? y)))
                                (.add data-series (double x) (double y))))
                            __x __y))
                      (.addSeries dataset data-series))

           chart  (create-plot
                   title
                   x-lab
                   y-lab
                   dataset
                   legend?
                   true  ; tooltips
                   false))
           _ (when x-groups
                (doseq [i (range 1 (count x-groups))]
                  (add-lines chart (nth x-groups i)
                             (nth y-groups i)
                             :series-label (cond
                                             _group-by
                                               (nth (reduce #(if (< (.indexOf %1 %2) 0) (conj %1 %2) %1) [] _group-by) i)
                                             series-lab
                                               series-lab
                                             :else
                                               (format "%s, %s (%s)" 'x 'y i))
                             :points points?)))]
      (.setRenderer (.getPlot chart) 0 (XYLineAndShapeRenderer. true points?))
      (set-theme chart theme)
      chart)))            

(comment
  (def ds (incanter.core/dataset [:start :quantity :cat]
                                 [{:start 0 :quantity 10 :cat :a}
                                  {:start 0 :quantity 2 :cat :b}
                                  {:start 10 :quantity 2 :cat :a}
                                  {:start 10 :quantity 5 :cat :b}]))
)

