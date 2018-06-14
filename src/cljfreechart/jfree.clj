(ns cljfreechart.jfree)

;;TBD
;;this only works for current dataset!
;;revise to work with series-seq....
(defn remove-series
  "Remove an existing series speicified by series-lab.
   If the series does not exist it return nil"
  [chart series-label]
  (let [data-set (-> chart .getPlot .getDataset)
        series   (try (.getSeries data-set series-label)
                      (catch UnknownKeyException e nil))]
    (when series
      (.removeSeries data-set series)
      chart)))

(defn has-series?
  "Test to see if a chart has a series name series-lab"
  [chart series-label]
  (try
    (-> chart .getPlot .getDataset (.getSeries series-label))
    true
    (catch UnknownKeyException e
      false)))

;;review
(defn fire-dataset-changed! [obj]
  (do  (invoke! org.jfree.data.general.AbstractDataset 
                obj 
                fireDatasetChanged)
       obj))
