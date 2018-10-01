;;The supplemental strategy for porting
;;jfreechart 1.5 is to wrap the factory/builder
;;methods for each chart type.

;;http://www.jfree.org/jfreechart/api/javadoc/index.html

;;This should expose, via a top-down fashion,
;;a mechanism for

(ns cljfreechart.factory
  (:import [org.jfree.chart ChartFactory JFreeChart]
           [org.jfree.data.general PieDataset]))

#_(def nodes
  (let [front  #(subs % 0 (dec (count %)))]
    (for [xs ls]
      (let [comments (keep #(when (clojure.string/starts-with? % ";") %) xs)
            meths    (keep #(when (clojure.string/starts-with? % "(") %) xs)
            name     (try (nth (clojure.string/split (first meths) #"\s+") 2)
                          (catch Exception e :undefined))]
        {:name name
         :comments comments
         :body (str (clojure.string/join \newline comments) \newline
                    (clojure.string/replace  (first meths)  #"\[.*\]"
                                             (apply str
                                                    (for [m meths]
                                                      (let [hargs (re-find #"\[.*\]" m)
                                                       args (->>
                                                             (clojure.string/split hargs #"\s")
                                                             (keep #(when (or (clojure.string/ends-with? % "," )
                                                                              (clojure.string/ends-with? % "]"))
                                                                      (front %)))
                                                             (clojure.string/join " "))]
                                                   (str "(" hargs  \newline "(ChartFactory/"
                                                        name " " args "))" \newline))))))}))))

;;Creates an area chart with default settings.
(defn ^JFreeChart create-area-chart
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel, ^CategoryDataset dataset])
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,
    ^CategoryDataset dataset, ^PlotOrientation orientation, ^boolean legend,
    ^boolean tooltips, ^boolean urls]))

;;Creates a bar chart with a vertical orientation.
;;Creates a bar chart.
(defn ^JFreeChart createBarChart
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel, ^CategoryDataset dataset]
   (ChartFactory/createBarChart title categoryAxisLabel valueAxisLabel dataset))
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,
    ^CategoryDataset dataset, ^PlotOrientation orientation, ^boolean legend,
    ^boolean tooltips, ^boolean urls]
   (ChartFactory/createBarChart title categoryAxisLabel valueAxisLabel dataset
                                orientation legend tooltips urls)))

;;Creates and returns a default instance of a box and whisker chart based on
;;data from a BoxAndWhisker^CategoryDataset.
;;Creates and returns a default instance of a box and whisker chart.

(defn ^JFreeChart  createBoxAndWhiskerChart
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,
    ^BoxAndWhiskerCategoryDataset dataset, ^boolean legend]
   (ChartFactory/createBoxAndWhiskerChart title categoryAxisLabel valueAxisLabel
   dataset legend))
  ([^String title, ^String timeAxisLabel, ^String valueAxisLabel,
    ^BoxAndWhiskerXYDataset dataset, ^boolean legend]
   (ChartFactory/createBoxAndWhiskerChart title timeAxisLabel valueAxisLabel dataset legend)))


;;Creates a bubble chart with default settings.
;;Creates a bubble chart with default settings.
(defn ^JFreeChart  createBubbleChart
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, XYZDataset dataset]
   (ChartFactory/createBubbleChart title xAxisLabel yAxisLabel dataset))
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, XYZDataset dataset,
    ^PlotOrientation orientation, ^boolean legend, ^boolean tooltips, ^boolean
    urls]
   (ChartFactory/createBubbleChart title xAxisLabel yAxisLabel dataset orientation legend tooltips urls)))

;;Creates and returns a default instance of a candlesticks chart.
;;Creates a Gantt chart using the supplied attributes plus default values where required.
(defn ^JFreeChart createCandlestickChart
  ([^String title, ^String timeAxisLabel, ^String valueAxisLabel, ^OHLCDataset
     dataset, ^boolean legend]
   (ChartFactory/createCandlestickChart title timeAxisLabel valueAxisLabel dataset legend))
  ([^String title, ^String categoryAxisLabel, ^String dateAxisLabel,
    ^IntervalCategoryDataset dataset]
   (ChartFactory/createCandlestickChart title categoryAxisLabel dateAxisLabel
                                        dataset)))

;;Creates a Gantt chart using the supplied attributes plus default values where
;;required. Creates and returns a default instance of a high-low-open-close
;;chart.
(defn ^JFreeChart  createGanttChart
  ([^String title, ^String categoryAxisLabel, ^String dateAxisLabel,
    ^IntervalCategoryDataset dataset, ^boolean legend, ^boolean tooltips, ^boolean
    urls]
   (ChartFactory/createGanttChart title categoryAxisLabel dateAxisLabel dataset legend tooltips urls))
  ([^String title, ^String timeAxisLabel, ^String valueAxisLabel,
    ^OHLCDataset dataset, ^boolean legend]
   (ChartFactory/createGanttChart title timeAxisLabel valueAxisLabel dataset
                                  legend)))

;;Creates a histogram chart.
(defn ^JFreeChart  createHistogram
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^IntervalXYDataset
    dataset]
   (ChartFactory/createHistogram title xAxisLabel yAxisLabel dataset))
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, IntervalXYDataset
    dataset, ^PlotOrientation orientation, ^boolean legend, ^boolean tooltips,
    ^boolean urls]
   (ChartFactory/createHistogram title xAxisLabel yAxisLabel dataset orientation
   legend tooltips urls)))

;;Creates a line chart with default settings.
;;Creates a line chart with default settings.
(defn ^JFreeChart  createLineChart
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,
    ^CategoryDataset dataset]
    (ChartFactory/createLineChart title categoryAxisLabel valueAxisLabel dataset))
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,
    ^CategoryDataset dataset, ^PlotOrientation orientation, ^boolean legend,
    ^boolean tooltips, ^boolean urls]
   (ChartFactory/createLineChart title categoryAxisLabel valueAxisLabel dataset
   orientation legend tooltips urls)))

;;Creates a chart that displays multiple pie plots.
;;Creates a chart that displays multiple pie plots.
(defn ^JFreeChart  createMultiplePieChart
  ([^String title, ^CategoryDataset dataset, ^TableOrder order, ^boolean legend,
   ^boolean tooltips, ^boolean urls]
   (ChartFactory/createMultiplePieChart title dataset order legend tooltips urls))
  ([^String title, ^CategoryDataset dataset, ^TableOrder order, ^boolean legend,
    ^boolean tooltips, ^boolean urls]
   (ChartFactory/createMultiplePieChart title dataset order legend tooltips urls)))

;;Creates a pie chart with default settings.
;;Creates a pie chart with default settings.
(defn ^JFreeChart  createPieChart
  ([^String title, ^PieDataset dataset]
   (ChartFactory/createPieChart title dataset))
  ([^String title, ^PieDataset dataset, ^boolean legend, ^boolean tooltips, ^boolean urls]
   (ChartFactory/createPieChart title dataset legend tooltips urls)))

;;Creates a pie chart with default settings.
;;Creates a pie chart with default settings that compares 2 datasets.
(defn ^JFreeChart  createPieChart
  ([^String title, ^PieDataset dataset, ^boolean legend, ^boolean tooltips, ^Locale locale]
   (ChartFactory/createPieChart title dataset legend tooltips locale))
  ([^String title, ^PieDataset dataset, ^PieDataset previousDataset, ^int
    percentDiffForMaxScale, ^boolean greenForIncrease, ^boolean legend, ^boolean
    tooltips, ^boolean urls, ^boolean subTitle, ^boolean showDifference]
   (ChartFactory/createPieChart title dataset previousDataset
   percentDiffForMaxScale greenForIncrease legend tooltips urls subTitle
   showDifference)))

;;Creates a pie chart with default settings that compares 2 datasets.
;;Creates a 3D pie chart using the specified dataset.
(defn ^JFreeChart  createPieChart
  ([^String title, ^PieDataset dataset, ^PieDataset previousDataset, ^int
  percentDiffForMaxScale, ^boolean greenForIncrease, ^boolean legend, ^boolean
  tooltips, ^Locale locale, ^boolean subTitle, ^boolean showDifference]
   (ChartFactory/createPieChart title dataset previousDataset
   percentDiffForMaxScale greenForIncrease legend tooltips locale subTitle
   showDifference))
  ([^String title, PieDataset dataset]
   (ChartFactory/createPieChart title dataset)))

;;Creates a 3D pie chart using the specified dataset.
;;Creates a 3D pie chart using the specified dataset.
(defn ^JFreeChart  createPieChart3D([^String title, PieDataset dataset, ^boolean legend, ^boolean tooltips, ^boolean urls]
(ChartFactory/createPieChart3D[^String title dataset legend tooltips urls))
([^String title, PieDataset dataset, ^boolean legend, ^boolean tooltips, Locale locale]
(ChartFactory/createPieChart3D[^String title dataset legend tooltips locale))
 ) 
;;Creates a polar plot for the specified dataset (x-values interpreted as angles in degrees] ).
;;Creates a ring chart with default settings.
(defn ^JFreeChart  createPolarChart ([^String title, ^XYDataset dataset, ^boolean legend, ^boolean tooltips, ^boolean urls]
(ChartFactory/createPolarChart title dataset legend tooltips urls))
([^String title, PieDataset dataset, ^boolean legend, ^boolean tooltips, ^boolean urls]
(ChartFactory/createPolarChart title dataset legend tooltips urls))
 ) 
;;Creates a ring chart with default settings.
;;Creates a scatter plot with default settings.
(defn ^JFreeChart  createRingChart ([^String title, PieDataset dataset, ^boolean legend, ^boolean tooltips, Locale locale]
(ChartFactory/createRingChart title dataset legend tooltips locale))
([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset]
(ChartFactory/createRingChart title xAxisLabel yAxisLabel dataset))
 ) 
;;Creates a scatter plot with default settings.
;;Creates a stacked area chart with default settings.
(defn ^JFreeChart  createScatterPlot ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset, ^PlotOrientation orientation, ^boolean legend, ^boolean tooltips, ^boolean urls]
(ChartFactory/createScatterPlot title xAxisLabel yAxisLabel dataset orientation legend tooltips urls))
([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,  ^CategoryDataset dataset]
(ChartFactory/createScatterPlot title categoryAxisLabel valueAxisLabel dataset))
 ) 
;;Creates a stacked area chart with default settings.
;;Creates a stacked bar chart with default settings.
(defn ^JFreeChart  createStackedAreaChart ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,  ^CategoryDataset dataset, ^PlotOrientation orientation, ^boolean legend, ^boolean tooltips, ^boolean urls]
(ChartFactory/createStackedAreaChart title categoryAxisLabel valueAxisLabel dataset orientation legend tooltips urls))
([^String title, ^String domainAxisLabel, ^String rangeAxisLabel,  ^CategoryDataset dataset]
(ChartFactory/createStackedAreaChart title domainAxisLabel rangeAxisLabel dataset))
 ) 
;;Creates a stacked bar chart with default settings.
;;Creates a stacked XY area plot.
(defn ^JFreeChart  createStackedBarChart ([^String title, ^String domainAxisLabel, ^String rangeAxisLabel,  ^CategoryDataset dataset, ^PlotOrientation orientation, ^boolean legend, ^boolean tooltips, ^boolean urls]
(ChartFactory/createStackedBarChart title domainAxisLabel rangeAxisLabel dataset orientation legend tooltips urls))
([^String title, ^String xAxisLabel, ^String yAxisLabel, TableXYDataset dataset]
(ChartFactory/createStackedBarChart title xAxisLabel yAxisLabel dataset))
 ) 
;;Creates a stacked XY area plot.
;;Creates and returns a time series chart.
(defn ^JFreeChart  createStackedXYAreaChart ([^String title, ^String xAxisLabel, ^String yAxisLabel, TableXYDataset dataset, ^PlotOrientation orientation, ^boolean legend, ^boolean tooltips, ^boolean urls]
(ChartFactory/createStackedXYAreaChart title xAxisLabel yAxisLabel dataset orientation legend tooltips urls))
([^String title, ^String timeAxisLabel, ^String valueAxisLabel, ^XYDataset dataset]
(ChartFactory/createStackedXYAreaChart title timeAxisLabel valueAxisLabel dataset))
 ) 
;;Creates and returns a time series chart.
;;Creates a wafer map chart.
(defn ^JFreeChart  createTimeSeriesChart ([^String title, ^String timeAxisLabel, ^String valueAxisLabel, ^XYDataset dataset, ^boolean legend, ^boolean tooltips, ^boolean urls]
(ChartFactory/createTimeSeriesChart title timeAxisLabel valueAxisLabel dataset legend tooltips urls))
([^String title, WaferMapDataset dataset, ^PlotOrientation orientation, ^boolean legend, ^boolean tooltips, ^boolean urls]
(ChartFactory/createTimeSeriesChart title dataset orientation legend tooltips urls))
 ) 
;;Creates a waterfall chart.
;;Creates a wind plot with default settings.
(defn ^JFreeChart  createWaterfallChart ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,  ^CategoryDataset dataset, ^PlotOrientation orientation, ^boolean legend, ^boolean tooltips, ^boolean urls]
(ChartFactory/createWaterfallChart title categoryAxisLabel valueAxisLabel dataset orientation legend tooltips urls))
([^String title, ^String xAxisLabel, ^String yAxisLabel, WindDataset dataset, ^boolean legend, ^boolean tooltips, ^boolean urls]
(ChartFactory/createWaterfallChart title xAxisLabel yAxisLabel dataset legend tooltips urls))
 ) 
;;Creates an area chart using an ^XYDataset.
;;Creates an area chart using an ^XYDataset.
(defn ^JFreeChart  createXYAreaChart ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset]
(ChartFactory/createXYAreaChart title xAxisLabel yAxisLabel dataset))
([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset, ^PlotOrientation orientation, ^boolean legend, ^boolean tooltips, ^boolean urls]
(ChartFactory/createXYAreaChart title xAxisLabel yAxisLabel dataset orientation legend tooltips urls))
 ) 


;;Creates a line chart (based on an ^XYDataset] ) with default settings.
(defn ^JFreeChart  createXYLineChart
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset])
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset,
   ^PlotOrientation orientation, ^boolean legend, ^boolean tooltips, ^boolean
  urls]))

;;Creates a filled stepped XY plot with default settings.
(defn ^JFreeChart  createXYStepAreaChart
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset] )
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset,
    ^PlotOrientation orientation, ^boolean legend, ^boolean tooltips, ^boolean
    urls]))

;;Creates a stepped XY plot with default settings.
(defn ^JFreeChart  createXYStepChart
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset] )
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset,
    ^PlotOrientation orientation, ^boolean legend, ^boolean tooltips, ^boolean urls]))


;;Returns the current chart theme used by the factory.
(defn ^ChartTheme getChartTheme [])

;;Sets the current chart theme.
(defn setChartTheme [^ChartTheme theme])




