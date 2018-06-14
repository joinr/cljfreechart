(ns cljfreechart.legend)

(defn ->paint-scale-legend [^PaintScale scale ^ValueAxis axis]
  (org.jfree.chart.title.PaintScaleLegend. scale axis))


(defprotocol ILegendItem
  (as-legend-item [x])) 

;;dumb place-holder for custom legends. Damn!
(defn ->rect [x y w h]
  (java.awt.Rectangle. x y w h))

(def legend-item-defaults
  {:shape       (->rect 0 0 20 20)
   :description ""
   :tooltip     ""
   :url         ""})

(defn ^LegendItem ->legend-item
  ([text shape color description tooltip url]
   (LegendItem. (str text) (str description) (str tooltip) (str url)
                shape (chart-color color)))
  ([m]
   (let [{:keys [text shape color description tooltip url]}
             (merge legend-item-defaults m)]
     (->legend-item text shape color description tooltip url))))

(extend-protocol ILegendItem
  LegendItem
  (as-legend-item [x] x)
  clojure.lang.PersistentArrayMap
  (as-legend-item [m] (->legend-item m))
  clojure.lang.PersistentHashMap
  (as-legend-item [m] (->legend-item m))) 

(defn ->discrete-legend
  "Given a sequence of maps, creates a discrete legend that corresponds to
   the items specified by the maps.  Items should be of the form
   {:keys [text shape color]}."
  [items]
  (let [^LegendItemCollection  coll
           (reduce (fn [^LegendItemCollection acc x]             
                     (doto acc (.add  (as-legend-item x))))
                   (LegendItemCollection.) items)
        source    (reify
                    LegendItemSource
                    (^LegendItemCollection getLegendItems [this] coll))]
    (LegendTitle. source)))

;;this fails, since discrete-het-legend is in charts.heat
(defn cobble-legend
  ([chart options]
   (-> (for [{:keys [legend-item key]} (series-details (.getPlot chart))]
          {:text key
           :shape (.getShape legend-item)
           :color (.getFillPaint legend-item)
           :line-color (.getLinePaint legend-item)
           }) 
       #_(c/->discrete-heat-legend options)
       (legend/->discrete-heat-legend options)))
  ([chart] (cobble-legend chart {})))

(defn add-derived-legend
  ([chart options]
   (let [legend (cobble-legend chart options)]
     (.addSubtitle chart legend)
     chart
     ))
  ([chart] (add-derived-legend chart {})))
