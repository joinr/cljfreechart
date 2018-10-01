(ns cljfreechart.override
  (:require [incanter core charts]
            [proc stacked])
  (:import [org.jfree.chart.annotations CategoryAnnotation
            CategoryLineAnnotation TextAnnotation]
           [org.jfree.chart.plot Plot]
           [org.jfree.chart.axis CategoryAnchor]
           [org.jfree.data.xy DefaultTableXYDataset XYSeries]
           [org.jfree.chart ChartFactory]))

;;Temporary name space for adding/changing functionality of
;;Jfreecharts, will eventually integrate into another ns


;; ==FUNCTIONS FOR CHANGING CATEGORY LINE ANNOTATIONS ON BAR CHARTS==
;;Need to find a way to change how CategoryLineAnnotations works.
;;When given a two categories, draws a line from the middle of the
;;first category to the middle of the second.  Have to change this to
;;accept a single category and draw the line from the the category
;;start to the category end.  These values can be found by
;;calling
;;(.getCategoryStart/End categoryPlot i i-count (java.awt.Rectange. 500 400)
;;   (.getDomainAxisEdge categoryPlot)) which returns a double.

;;Still have to find a way to change the end/start location on the
;; line annotations, don't know which draw method has to be overriden
;; sources:
;; http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/annotations/CategoryLineAnnotation.html
;; http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/plot/

;; Still need to find out what method the draw method of
;; CategoryAnnotation uses internally.  I assume internally it calls
;; find middle on category c1 and category c2 then calls someother
;; draw inherited draw function.

;;Source (java) for draw method from:
;;https://github.com/jfree/jfreechart/blob/master/src/main/java/org/jfree/chart/annotations/CategoryLineAnnotation.java
;;Need to override this with proxy to change line annotation behavior
;;for single categories
" public void draw(Graphics2D g2, CategoryPlot plot, Rectangle2D dataArea,
                     CategoryAxis domainAxis, ValueAxis rangeAxis) {
        CategoryDataset dataset = plot.getDataset();
        int catIndex1 = dataset.getColumnIndex(this.category1);
        int catIndex2 = dataset.getColumnIndex(this.category2);
        int catCount = dataset.getColumnCount();
        double lineX1 = 0.0f;
        double lineY1 = 0.0f;
        double lineX2 = 0.0f;
        double lineY2 = 0.0f;
        PlotOrientation orientation = plot.getOrientation();
        RectangleEdge domainEdge = Plot.resolveDomainAxisLocation(
            plot.getDomainAxisLocation(), orientation);
        RectangleEdge rangeEdge = Plot.resolveRangeAxisLocation(
            plot.getRangeAxisLocation(), orientation);

        if (orientation == PlotOrientation.HORIZONTAL) {
            lineY1 = domainAxis.getCategoryJava2DCoordinate(
                CategoryAnchor.MIDDLE, catIndex1, catCount, dataArea,
                domainEdge);
            lineX1 = rangeAxis.valueToJava2D(this.value1, dataArea, rangeEdge);
            lineY2 = domainAxis.getCategoryJava2DCoordinate(
                CategoryAnchor.MIDDLE, catIndex2, catCount, dataArea,
                domainEdge);
            lineX2 = rangeAxis.valueToJava2D(this.value2, dataArea, rangeEdge);
        }
        else if (orientation == PlotOrientation.VERTICAL) {
            lineX1 = domainAxis.getCategoryJava2DCoordinate(
                CategoryAnchor.MIDDLE, catIndex1, catCount, dataArea,
                domainEdge);
            lineY1 = rangeAxis.valueToJava2D(this.value1, dataArea, rangeEdge);
            lineX2 = domainAxis.getCategoryJava2DCoordinate(
                CategoryAnchor.MIDDLE, catIndex2, catCount, dataArea,
                domainEdge);
            lineY2 = rangeAxis.valueToJava2D(this.value2, dataArea, rangeEdge);
        }
        g2.setPaint(this.paint);
        g2.setStroke(this.stroke);
        g2.drawLine((int) lineX1, (int) lineY1, (int) lineX2, (int) lineY2);
    }
"
;;Will return a CategoryLineAnnotation proxy:
;;-the draw method will draw a line from the starting edge of category
;; c1 to the ending edge of category c1 at value v1
;;-a second category and value can be given as optional arguments (c1
;; always start edge, c2 always end edge) for the line
;;-where c1,c2 are anchored to the domain axis and v1,v2 are anchored
;; to the range axis.
;;-paint is a java Color object which determines the color of the line
;;-stroke is a java Stroke object which determines the stroke used to
;;-draw the line
(defn annotation
  [chart c1 v1 & {:keys [paint stroke c2 v2]
                  :or {c2 c1 v2 v1 paint (java.awt.Color/black)
                       stroke (java.awt.BasicStroke. 3 2 2)}}]
  (proxy
      [org.jfree.chart.annotations.CategoryLineAnnotation]
      [c1 v1 c2 v2 paint stroke]
    ;;Graphics2D g2 and Rectange2D dataArea automatically supplied by graphics interface
    (draw [^java.awt.Graphics g2 cp dataArea da va]
      (let [cp (.getCategoryPlot chart)
            ra dataArea
            da (.getDomainAxis cp) ;;domain axis
            va (.getRangeAxis cp) ;;value axis
            dset (.getDataset cp) ;;categorical data set
            catIndex (.getColumnIndex dset c1) ;;category index
            catCount (.getColumnCount dset) ;;numer of categories total
            domainEdge (org.jfree.chart.plot.Plot/resolveDomainAxisLocation
                        (.getDomainAxisLocation cp) (.getOrientation cp))
            rangeEdge (org.jfree.chart.plot.Plot/resolveRangeAxisLocation
                       (.getRangeAxisLocation cp) (.getOrientation cp))
            startAnchor (org.jfree.chart.axis.CategoryAnchor/START)
            endAnchor (org.jfree.chart.axis.CategoryAnchor/END)
            y1 (.valueToJava2D va v1 ra rangeEdge) ;;y1 = y2
            x1 (.getCategoryJava2DCoordinate da startAnchor
                    catIndex catCount ra domainEdge)
            x2 (.getCategoryJava2DCoordinate da endAnchor
                    catIndex catCount ra domainEdge)]
        (.setPaint g2 paint)
        (.setStroke g2 stroke)
        (.drawLine g2 x1 y1 x2 y1)))))

;;Default JFree category line annotation behavior wrapper.
;;Will add a black line at the point [category, value] on the chart. 
;;Because category1 = category2, the line will be a single point, 
;; -CategoryPlots don't support just drawing a regular line of some
;;  specified length; (USE annotation function for single category
;;  line)
;; -can only draw a line between/across catagories because of the
;;  level of abstraction in jfree.chart.annotations
(defn add-category-line [chart category value]
  (.addAnnotation (.getCategoryPlot chart)
    (org.jfree.chart.annotations.CategoryLineAnnotation.
     category value category value 
     (java.awt.Color/black)
     (java.awt.BasicStroke. 2 2 2 1 (float-array 3 1) 1))))

;; ===============================================================
;; ========== FUNCTIONS FOR STACKED AREA CHARTS ==================

;;***map-list = list of maps using keys from first line (header) of file

;;Groups data map-list by key (String)
;;Returns multi-demensional list of map-list grouped by key, where key
;;is the name of a column in original data (String)
(defn group-by-key [map-list key]
  (let [k (keyword key)]
    (group-by k map-list)))

;;Returns set of records that fall into the given time periods.
;;Map-list is a single instance of a map list (a single entry after
;;calling group-by-key or an entry after file->map-list if ungrouped)
;;   Example: map-list =
;;   ({:key "k1" :start 0 :duration 100 :otherdata "data"}
;;    {:key "k2" :start 25 :duration :50 :otherdata "more data")
;;-Start and end are the time period bondaries (record falls within
;; period [start, end] inclusive)
;;-startkeyfn and endkeyfn are functions that can be called on a
;; single data map and will return the field that represents the
;; records start time/end time
;;   Example: startkeyfn = #(read-num (:StartDay %))
;;            endkeyfn = #(+ (read-num (:Duration %)) (read-num (:StartDay %)))
;; *startkeyfn and endkeyfn are expected to return numerical values (not strings)
(defn in-period [map-list start end startkeyfn endkeyfn]
  (filter #(and (>= start (startkeyfn  %)) (<= end (endkeyfn %))) map-list))

;;Returns the y value to be used for the record at x value (time) of time
;;Map list is a single instance of a map list
;;  (ex: ({:key "k1" :start 0 :duration 100 :otherdata "data"}
;;       {:key "k2" :start 25 :duration :50 :otherdata "more data"))
;;ykeyfn is a function that gets the y-value from a single record (map)
;;   Example: #(* (read-num (:Quantity %)) (read-num (:People %)))
(defn y-at-time [map-list time ykeyfn startkeyfn endkeyfn]
  (reduce + (map ykeyfn (in-period map-list time (inc time) startkeyfn endkeyfn))))

;;Calculates the local start time of the map-list.
;;startkeyfn is a function that returns the start time from the map
;;*as a numerical value (not string)
(defn local-start [map-list startkeyfn]
  (reduce min (for [m map-list] (startkeyfn m))))

;;Calculates the local end time of the map-list.
;;endkeyfn is a function that returns the end time from a map *as a
;;numerical value (not string)
;;   Example: endkeyfn = #(+ (read-num (:Duration %)) (read-num (:StartDay %)))
(defn local-end [map-list endkeyfn]
  (reduce max (for [m map-list] (endkeyfn m))))

;;Calculates the global start time by finding the minimum of all local
;;start times.
(defn global-start [grouped-map-list startkeyfn]
  (reduce min
    (for [k (keys grouped-map-list) :let [map-list (get grouped-map-list k)]]
     (local-start map-list startkeyfn))))
    
;;Calculates the global start time by finding the minimum of all local
;;start times.
(defn global-end [grouped-map-list endkeyfn]
  (reduce max
    (for [k (keys grouped-map-list) :let [map-list (get grouped-map-list k)]]
      (local-end map-list endkeyfn))))

;;Returns a sequence of time values that are distinct in the dataset
(defn distinct-times [map-list startfn endfn]
  (sort (distinct (apply conj (map startfn map-list) (map endfn map-list)))))

;;Returns map of xy-pairs with key as time and value as quantity (only
;;includes distinct times)
(defn xy-pairs [map-list yfn startfn endfn]
  (let [dtimes (distinct-times map-list startfn endfn)]
    (reduce conj
       (pmap #(identity {% (y-at-time map-list % yfn startfn endfn)})
             dtimes))))

;;Repeates y value from x1 to x2-1 for all xs in xypairs
;; where both xypairs and expairs are maps with x as key and y as
;; val (xpairs initiall empty map)
(defn expand-xy-pairs [xypairs expairs]
 (let [k (sort (keys xypairs)) a (first k) b (second k)]
   (if (nil? b)
     (conj xypairs expairs) ;;base case, combine xypairs and expairs
     (expand-xy-pairs (dissoc xypairs a) ;;expand 
       (conj expairs (zipmap 
                       (range a b) 
                       (repeat (- b a) (get xypairs a))))))))

;;XY-pairs is a map with x value as key and y value as val
(defn expand-all [xy-pairs globalstart globalend]
  (let [s (get xy-pairs globalstart) 
        e (get xy-pairs globalend)
        xyp (apply concat 
              (list [[globalstart (if (nil? s) 0 s)]] 
                    (filter #(and (< globalstart (first %))
                                  (> globalend (first %)))
                            (sort xy-pairs))
                    [[globalend (if (nil? e) 0 e)]]))]
    (sort (expand-xy-pairs
           (zipmap (map first xyp)
                   (map second xyp)) {}))))

;;Returns an instance of DefaultTableXYDataset.
;; getAllowDuplicateXValues will also returns false, do not use if
;; there will be x duplicated
;;Used to create the internal structure of datastructures used in
;;jfree chart.
;;XY series have a list of xy pairs and a key associated with the
;;series.
(defn ->xyseries [grouped-map-list key yfn startfn endfn]
  (let [pairs (expand-all 
               (xy-pairs (get grouped-map-list key)
                         yfn startfn endfn) 
               (global-start grouped-map-list startfn) 
               (global-end grouped-map-list endfn))
        ;;DefaultTableXYDataset cannot accept Series which allow
        ;;duplicate x values already made sure no duplicate xs by
        ;;using hash-map with x as key always return false for
        ;;getAllowDuplicateXValues method
        series (proxy [org.jfree.data.xy.XYSeries] [key]
                 (getAllowDuplicateXValues ([] false)))] 
    (doseq [xy pairs]
      (.add series (first xy) (second xy)))
    series))

;;Returns a DefaultTavleXYDataset which can be used to build a
;;stackedXYChart
(defn ->ds [map-list groupby-key yfn startfn endfn]
  (let [grouped-map-list (group-by-key map-list groupby-key)
        k (keyword groupby-key)
        keys (set (map k map-list))
        series (map #(->xyseries grouped-map-list %
                                 yfn startfn endfn) keys)
        ds (org.jfree.data.xy.DefaultTableXYDataset.)]
    (doseq [s series] (.addSeries ds s))
    ds))   

;;JFree chart wrapper for creating a stacked-xy-chart
;;ds is a some instance of the TabelXYDataset interface
(defn stacked-xy-chart
  [ds & {:keys [title x-label y-label]
         :or {title "" x-label "time" y-label "quantity"}}]
  (org.jfree.chart.ChartFactory/createStackedXYAreaChart
   title x-label y-label ds))

;;Sets the colors of a Stack-XY-Sand chart according to map.
;;Chart is a Jfreechart obj containing some XYPlots.
;;color-map is a map with the series key as the key and a color
;;keyword (:red, :blue, ect. -- see proc.stacked for all colors) as
;;the value
(defn set-xysand-colors [chart color-map]
  (when color-map
    (proc.stacked/set-colors chart color-map))
  chart)

;;Given a Jfree table dataset, returns the series object given the
;;series key
(defn get-series-by-key [dataset key]
  (.getSeries dataset 
     (first (filter #(= key (.getSeriesKey dataset %))
                    (range (.getSeriesCount dataset))))))

;;Returns a map with series key as key and series Jfreeobject as value 
(defn series-map [dataset]
  (let [pairs (map #(vector (.getSeriesKey dataset %)
                            (.getSeries dataset %))
                   (range (.getSeriesCount dataset)))]
    (zipmap (map first pairs) (map second pairs))))

;;Orders the sand charts according to ordering (sequence of series
;;keys where the nth element will appear nth on the chart)
;;***The entire ordering has to be defined, if something is not in the
;;***ordering, it will not appear on the chart.
(defn sort-series [chart ordering]
  (let [plot (.getXYPlot chart)]
    (doseq [n (range (.getDatasetCount plot))]
      (let [dataset (.getDataset plot n)
            smap (series-map dataset)]
        (.removeAllSeries dataset)
        (doseq [o ordering]
          (.addSeries dataset (get smap o))))))
  chart) 


