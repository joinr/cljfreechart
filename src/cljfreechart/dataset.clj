(ns cljfreechart.dataset)


(declare       stacked-areaxy-chart)

;;defining XY series
;;==================
(defn series-by
  "Given key functions for x,y, and series, processes
   collection xs, returning a map of
  {series-key series}
   with meta for ordering purposes:
   {:order {:idx->key [n series-key]
            :key->idx [series-key n]}}"
  [x-key y-key series-key xs]
  (let [order       (atom [])
        get-series! (fn get-series! ^XYSeries [series k] 
                      (if-let [res (get @series k)]
                        res
                        (let [s (XYSeries. k true false)
                              _ (swap! series assoc k s)
                              _ (swap! order (fn [xs] (conj xs [(count xs) k])))]
                          s)))]
    (with-meta   (->> xs
                      (reduce (fn [acc r]
                                (let [^XYSeries s (get-series! acc (series-key r))]
                                  (do (.add s ^double (x-key r) ^double (y-key r))
                                      acc))) (atom {}))
                      (deref))
      (let [os @order]
           {:order {:idx->key   (into {} os)
                    :key->order (into {} (map (fn [[l r]] [r l]) os))}}))))

;;Protocols extended to JFreeChart types..

;;there's some weirdness with the duplicate value error, I'm just
;;going to trap it with a try-catch,finally and ignore it.
(extend-type   DefaultTableXYDataset
  IOrderedSeries
  (order-series-by [obj f] (order-series-by! obj f))
  IXYData
  (as-tablexy [obj] obj)
  (series     [obj nm]  (reduce (fn [acc idx] 
                                  (let [^XYSeries ser (.getSeries obj idx)
                                        k (.getKey ser)]
                                    (if (= k nm)
                                      (reduced ser)
                                      acc)))
                                nil 
                                (range (.getSeriesCount obj))))
  (series-seq [obj] (let [cnt (.getSeriesCount obj)]
                      (map (fn [idx] (let [s (.getSeries obj idx)]
                                       [(.getKey s) s]))
                           (range cnt))))
  (get-bounds [obj] (let [^XYSeries s (.getSeries obj 0)]
                      [(.getMinX s) (.getMaxX s)]))
  IReactiveData
  (set-notify [obj v] (let [v (boolean v)]
                        (doseq [[_ ^XYSeries ser] (series-seq obj)]
                          (.setNotify ser v))
                        obj))
  (add-samples [obj samples]
    (let [series-map (into {} (series-seq obj))]
      (do (set-notify obj false)
          (doseq [s samples]
            (doseq [[nm ^double x ^double y] s]
              (.addOrUpdate ^XYSeries (get series-map nm) x y)))  ;;use addOrUpdate to get around false dupes.
          (do! (set-notify obj true))
          (fire-dataset-changed! obj)
          ))))

(extend-type   XYSeriesCollection
  IOrderedSeries
  (order-series-by [obj order-func]
    (let [ofunc (as-order-function order-func)]
      (reduce (fn [^XYSeriesCollection acc  [nm ^XYSeries ser]]
                (doto acc (.addSeries ser)))
              (XYSeriesCollection.)    
              (sort-by ofunc (series-seq obj)))))
  IXYData
  (as-tablexy [obj] obj)
  (series     [obj nm]  (.getSeries obj nm))
  (series-seq [obj]     (map (fn [^XYSeries s] [(.getKey s) s]) (seq (.getSeries obj))))
  (get-bounds [obj]    (let [^XYSeries s (.getSeries obj 0)]
                         [(.getMinX s) (.getMaxX s)]))
  IReactiveData
  (set-notify [obj v] (let [v (boolean v)]
                        (doseq [[_ ^XYSeries ser] (series-seq obj)]
                          (.setNotify ser v))
                        obj))
  (add-samples [obj samples]
    (let [series-map (into {} (series-seq obj))]
      (do (set-notify obj false)
          (doseq [s samples]
            (doseq [[nm ^double x ^double y] s]
              (.addOrUpdate ^XYSeries (get series-map nm) x y)))  ;;use addOrUpdate to get around false dupes.


;;This should be more generic.
;;Do we not have the ability to order any XYDataset by its series?
(defn order-series-by!
  ""
  [^DefaultTableXYDataset tab order-func]
  (let [ofunc (as-order-function order-func)]
        (reduce (fn [^DefaultTableXYDataset acc [nm ^XYSeries ser]]
                  (doto acc (.addSeries ser)))
                (DefaultTableXYDataset.)    
                (sort-by ofunc (series-seq tab)))))

(defn derive-order
  "Given an XYData tab, derives the current order of the series in the
   dataset, returning a bi-directional mapping of idx->series name
   and series-name -> idx."
  [tab]
    (reduce (fn [{:keys [idx->key key->order]} [k ^XYSeries ser]]
              {:idx->key (assoc idx->key (count idx->key) k)
               :key->order (assoc key->order k (count idx->key))})
            {:idx->key {}
             :key->order {}}
            (series-seq tab)))

;;Building XY Datasets
;;====================

;;defining TableXYDatasets for stacked area charts, which is categorically
;;different (hah!) from the CategoryDataset.
(defn ^org.jfree.data.xy.DefaultTableXYDataset build-datatable
  "Creates a table dataset from a series-map (like output from series-by),
   which should be a map of {series-name ^XYSeries obj}
   and uses ordering metadata to add the series in order for the dataset.x"
  [series-map & {:keys [order-by]}]
  (let [{:keys [idx->key key->order]}   (get (meta series-map) :order)
        idxs   (range (count idx->key))
        trends (keys   key->order)
        order  (if order-by 
                 (sort-by order-by trends)  
                 (sort-by (fn [k] (key->order k)) trends))
        addf   (fn [^DefaultTableXYDataset ds ^XYSeries ser]                 
                 (doto ds (.addSeries ser)))]
    (reduce (fn [^DefaultTableXYDataset ds nm] 
              (let [^XYSeries series (get series-map nm) ]
                (addf ds series)))
            (org.jfree.data.xy.DefaultTableXYDataset.)
            order)))


(defrecord xydataset [^DefaultTableXYDataset table seriesmap order] ;this is like a java class
  ;this implements these protocols. A record is just a hash-map record with some fancy java stuff....
  ;Deftype if you don't care if it's a hashmap.
  ;reify and produce a bunch of protocols-Anoymous object that implements a bunch of protocols.
  IOrderedSeries
  (order-series-by [obj f]
    (let [tab  (order-series-by! table f)]
      (xydataset. tab seriesmap (derive-order tab))))
  IXYData
  (as-tablexy [obj] table)
  (series     [obj nm]  (get seriesmap nm))
  (series-seq [obj]     (series-seq table))
  (get-bounds [obj]   (let [^XYSeries s (first (vals seriesmap))]
                           [(.getMinX s) (.getMaxX s)]))
  IChartable 
  (as-chart [obj  opts] (apply stacked-areaxy-chart obj (if (map? opts) 
                                                            (flatten (seq opts))
                                                            opts)))
  IReactiveData
  (set-notify [obj v] (do (set-notify table v) obj))
  (add-samples [obj samples] (do (add-samples table samples) obj)))

;;Potentially OBE....incanter-specific?
(defn xy-table
  [xkey ykey & options]
    (let [opts         (when options (apply assoc {} options))
          data         (or (:data opts) $data)
          _group-by    (when (:group-by opts) (:group-by opts)) ; (data-as-list (:group-by opts) data)) 
          ;;new
          series-map   (series-by xkey ykey (or _group-by (fn [_] "Series")) (ds/row-maps data))
          ;;new
          order        (get (meta series-map) :order)
          dtbl         (build-datatable   series-map)]
      ;;the difference between the regular area and this guy is that
      ;;we have a category, defined by group-by, and the xs and ys....
      ;;I'll rename _categories to xs at some point and values to
      ;;ys......
      (->xydataset dtbl series-map order)))

