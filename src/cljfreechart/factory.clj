;;The supplemental strategy for porting
;;jfreechart 1.5 is to wrap the factory/builder
;;methods for each chart type.

;;http://www.jfree.org/jfreechart/api/javadoc/index.html

;;This should expose, via a top-down fashion,
;;a mechanism for

(ns cljfreechart.factory
  (:import [org.jfree.chart ChartFactory JFreeChart]
           [org.jfree.data.general PieDataset ]))


;;Noticing lots of similarities in arities and the like...

;;Categorical Charts
;;==================

;;We can look at the source further to dissect these.
;;For now, we'll use the simple builder interface.

;;CategoryDatset
;;Creates an area chart with default settings.
(defn ^JFreeChart createAreaChart
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel, ^CategoryDataset dataset]
   (ChartFactory/createAreaChart title categoryAxisLabel valueAxisLabel dataset))
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,
    ^CategoryDataset dataset, ^PlotOrientation orientation, ^Boolean legend,
    ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createAreaChart
    title, categoryAxisLabel,  valueAxisLabel, dataset,  orientation,  legend,
    tooltips,  urls)))

;;CategoryDatset
;;Creates a bar chart with a vertical orientation.
;;Creates a bar chart.
(defn ^JFreeChart createBarChart
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel, ^CategoryDataset dataset]
   (ChartFactory/createBarChart title categoryAxisLabel valueAxisLabel dataset))
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,
    ^CategoryDataset dataset, ^PlotOrientation orientation, ^Boolean legend,
    ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createBarChart title categoryAxisLabel valueAxisLabel dataset
                                orientation legend tooltips urls)))

;;BoxAndWhiskerCategory
;;Creates and returns a default instance of a box and whisker chart based on
;;data from a BoxAndWhisker^CategoryDataset.
(defn ^JFreeChart  createBoxAndWhiskerChart
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,
    ^BoxAndWhiskerCategoryDataset dataset, ^Boolean legend]
   (ChartFactory/createBoxAndWhiskerChart title categoryAxisLabel valueAxisLabel
                                          dataset legend)))

;;IntervalCategoryDataset
;;Creates a Gantt chart using the supplied attributes plus default values where
;;required. 
(defn ^JFreeChart  createGanttChart
  ;;IntervalCategoryDataset
  ([^String title, ^String categoryAxisLabel, ^String dateAxisLabel,
    ^IntervalCategoryDataset dataset]
   (ChartFactory/createGanttChart title categoryAxisLabel
                                        dateAxisLabel dataset))
  ([^String title, ^String categoryAxisLabel, ^String dateAxisLabel,
    ^IntervalCategoryDataset dataset, ^Boolean legend, ^Boolean tooltips, ^Boolean
    urls]
   (ChartFactory/createGanttChart title categoryAxisLabel dateAxisLabel
                                  dataset legend tooltips urls)))

;;CategoryDataset
;;Creates a line chart with default settings.
(defn ^JFreeChart  createLineChart
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,
    ^CategoryDataset dataset]
    (ChartFactory/createLineChart title categoryAxisLabel valueAxisLabel dataset))
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,
    ^CategoryDataset dataset, ^PlotOrientation orientation, ^Boolean legend,
    ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createLineChart title categoryAxisLabel valueAxisLabel dataset
                                 orientation legend tooltips urls)))

;;CategoryDataset
;;Creates a stacked area chart with default settings.
(defn ^JFreeChart  createStackedAreaChart
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,  ^CategoryDataset dataset]
   (ChartFactory/createStackedAreaChart title categoryAxisLabel valueAxisLabel dataset))
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,
    ^CategoryDataset dataset, ^PlotOrientation orientation, ^Boolean
    legend, ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createStackedAreaChart title categoryAxisLabel
    valueAxisLabel dataset orientation legend tooltips urls)))

;;CategoryDataset
;;Creates a stacked bar chart with default settings.
(defn ^JFreeChart  createStackedBarChart
  ([^String title, ^String domainAxisLabel, ^String rangeAxisLabel,  ^CategoryDataset dataset]
   (ChartFactory/createStackedBarChart title domainAxisLabel rangeAxisLabel dataset))
  ([^String title, ^String domainAxisLabel, ^String rangeAxisLabel,
    ^CategoryDataset dataset, ^PlotOrientation orientation,
    ^Boolean legend, ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createStackedBarChart title domainAxisLabel
    rangeAxisLabel dataset orientation legend tooltips urls)))

;;CategoryDataset
;;Creates a waterfall chart.
(defn ^JFreeChart  createWaterfallChart
  ([^String title, ^String categoryAxisLabel, ^String valueAxisLabel,
    ^CategoryDataset dataset, ^PlotOrientation orientation, ^Boolean legend, ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createWaterfallChart title categoryAxisLabel
                                      valueAxisLabel dataset orientation legend tooltips urls)))




;;CategoryDataset
;;Creates a chart that displays multiple pie plots.
(defn ^JFreeChart  createMultiplePieChart
  ([^String title, ^CategoryDataset dataset, ^TableOrder order, ^Boolean legend,
    ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createMultiplePieChart title dataset order legend tooltips urls))
  ([^String title, ^CategoryDataset dataset, ^TableOrder order, ^Boolean legend,
    ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createMultiplePieChart title dataset order legend tooltips urls)))

;;Pie Charts
;;==========

;;PieDataset
;;Creates a pie chart with default settings.
(defn ^JFreeChart  createPieChart
  ([^String title, ^PieDataset dataset]
   (ChartFactory/createPieChart title dataset))
  ([^String title, ^PieDataset dataset, ^Boolean legend, ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createPieChart title dataset legend tooltips urls))
  ([^String title, ^PieDataset dataset, ^Boolean legend, ^Boolean tooltips, ^Locale locale]
   (ChartFactory/createPieChart title dataset legend tooltips locale)))

;;PieDataset
;;Creates a pie chart with default settings that compares 2 datasets.
(defn ^JFreeChart createPieChartComparison
  ([^String title, ^PieDataset dataset, ^PieDataset previousDataset, ^int
    percentDiffForMaxScale, ^Boolean greenForIncrease, ^Boolean legend, ^Boolean
    tooltips, ^Boolean urls, ^Boolean subTitle, ^Boolean showDifference]
   (ChartFactory/createPieChart title dataset previousDataset
   percentDiffForMaxScale greenForIncrease legend tooltips urls subTitle
   showDifference))
  ([^String title, ^PieDataset dataset, ^PieDataset previousDataset, ^int
    percentDiffForMaxScale, ^Boolean greenForIncrease, ^Boolean legend, ^Boolean
    tooltips, ^Locale locale, ^Boolean subTitle, ^Boolean showDifference]
   (ChartFactory/createPieChart title dataset previousDataset
                                percentDiffForMaxScale greenForIncrease legend tooltips locale subTitle
                                showDifference)))

;;PieDataset
;;Creates a 3D pie chart using the specified dataset.
(defn ^JFreeChart  createPieChart3D
  ([^String title, ^PieDataset dataset]
   (ChartFactory/createPieChart3D title dataset))
  ([^String title, ^PieDataset dataset, ^Boolean legend, ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createPieChart3D title dataset legend tooltips urls))
  ([^String title, ^PieDataset dataset, ^Boolean legend, ^Boolean tooltips, ^Locale locale]
   (ChartFactory/createPieChart3D title dataset legend tooltips locale)))


;;PieDataset
;;Creates a ring chart with default settings.
(defn ^JFreeChart  createRingChart
  ([^String title, ^PieDataset dataset, ^Boolean legend, ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createRingChart title dataset legend tooltips urls))
  ([^String title, ^PieDataset dataset, ^Boolean legend, ^Boolean tooltips, ^Locale locale]
   (ChartFactory/createRingChart title dataset legend tooltips locale)))



;;XY/Z TableXY Datasets
;;=====================

;;BoxAndWhiskerXYDataset
;;Creates and returns a default instance of a box and whisker chart.
(defn ^JFreeChart  createBoxAndWhiskerChartXY
  ([^String title, ^String timeAxisLabel, ^String valueAxisLabel,
    ^BoxAndWhiskerXYDataset dataset, ^Boolean legend]
   (ChartFactory/createBoxAndWhiskerChart title timeAxisLabel
                                          valueAxisLabel dataset legend)))

;;XYZDataset
;;Creates a bubble chart with default settings.
(defn ^JFreeChart  createBubbleChart
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYZDataset dataset]
   (ChartFactory/createBubbleChart title xAxisLabel yAxisLabel dataset))
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYZDataset dataset,
    ^PlotOrientation orientation, ^Boolean legend, ^Boolean tooltips, ^Boolean
    urls]
   (ChartFactory/createBubbleChart title xAxisLabel yAxisLabel dataset
    orientation legend tooltips urls)))

;;OHLCDataset
;;Creates and returns a default instance of a candlesticks chart.
;;Creates a Gantt chart using the supplied attributes plus default values where required.
(defn ^JFreeChart createCandlestickChart
  ([^String title, ^String timeAxisLabel, ^String valueAxisLabel, ^OHLCDataset
     dataset, ^Boolean legend]
   (ChartFactory/createCandlestickChart title timeAxisLabel
                                        valueAxisLabel dataset legend)))

;;OHLCDataset
;;Creates and returns a default instance of a high-low-open-close chart.
(defn ^JFreeChart  createHighLowChart
  ([^String title, ^String timeAxisLabel, ^String valueAxisLabel,
    ^OHLCDataset dataset, ^Boolean legend]
   (ChartFactory/createHighLowChart title timeAxisLabel valueAxisLabel dataset
                                    legend)))

;;IntervalXYDataset
;;Creates a histogram chart.
(defn ^JFreeChart  createHistogram
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^IntervalXYDataset
    dataset]
   (ChartFactory/createHistogram title xAxisLabel yAxisLabel dataset))
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^IntervalXYDataset
    dataset, ^PlotOrientation orientation, ^Boolean legend, ^Boolean tooltips,
    ^Boolean urls]
   (ChartFactory/createHistogram title xAxisLabel yAxisLabel dataset orientation
   legend tooltips urls)))

;;XYDataset
;;Creates a polar plot for the specified dataset (x-values interpreted as angles in degrees).
(defn ^JFreeChart  createPolarChart
  ([^String title, ^XYDataset dataset, ^Boolean legend, ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createPolarChart title dataset legend tooltips urls)))

;XYDataset
;;Creates a scatter plot with default settings.
(defn ^JFreeChart  createScatterPlot
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset]
   (ChartFactory/createScatterPlot title xAxisLabel yAxisLabel dataset))
  ([^String title, ^String xAxisLabel, ^String yAxisLabel,
    ^XYDataset dataset, ^PlotOrientation orientation, ^Boolean legend,
    ^Boolean   tooltips, ^Boolean urls]
   (ChartFactory/createScatterPlot title xAxisLabel yAxisLabel dataset orientation legend tooltips urls)))

;;TableXYDataset
;;Creates a stacked XY area plot.
(defn ^JFreeChart  createStackedXYAreaChart
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^TableXYDataset dataset]
   (ChartFactory/createStackedXYAreaChart title xAxisLabel yAxisLabel dataset))
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^TableXYDataset dataset,
    ^PlotOrientation orientation, ^Boolean legend, ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createStackedXYAreaChart title xAxisLabel yAxisLabel
    dataset orientation legend tooltips urls)))

;;XYDataset
;;Creates and returns a time series chart.
(defn ^JFreeChart  createTimeSeriesChart
  ([^String title, ^String timeAxisLabel, ^String valueAxisLabel, ^XYDataset dataset]
   (ChartFactory/createTimeSeriesChart title timeAxisLabel valueAxisLabel dataset))
  ([^String title, ^String timeAxisLabel, ^String valueAxisLabel, ^XYDataset dataset,
    ^Boolean legend, ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createTimeSeriesChart title timeAxisLabel valueAxisLabel dataset legend tooltips urls))
  )

;;XYDataset
;;Creates an area chart using an XYDataset.
(defn ^JFreeChart  createXYAreaChart
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset]
   (ChartFactory/createXYAreaChart title xAxisLabel yAxisLabel dataset))
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset,
    ^PlotOrientation orientation, ^Boolean legend, ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createXYAreaChart title xAxisLabel yAxisLabel dataset orientation legend tooltips urls)))

;;XYDataset
;;Creates a bar chart  (based on an XYDataset) with default settings.
(defn ^JFreeChart  createXYBarChart
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset]
   (ChartFactory/createXYBarChart  title xAxisLabel yAxisLabel dataset))
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset
   dataset, ^PlotOrientation orientation, ^Boolean legend, ^Boolean
   tooltips, ^Boolean urls]
   (ChartFactory/createXYBarChart title xAxisLabel yAxisLabel dataset
                                  orientation legend tooltips urls)))

;;XYDataset
;;Creates a line chart (based on an XYDataset) with default settings.
(defn ^JFreeChart  createXYLineChart
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset]
   (ChartFactory/createXYLineChart  title xAxisLabel yAxisLabel dataset))
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset
   dataset, ^PlotOrientation orientation, ^Boolean legend, ^Boolean
   tooltips, ^Boolean urls]
   (ChartFactory/createXYLineChart title xAxisLabel yAxisLabel dataset
                                  orientation legend tooltips urls)))

;;XYDataset
;;Creates a filled stepped XY plot with default settings.
(defn ^JFreeChart  
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset]
   (ChartFactory/createXYStepAreaChart  title xAxisLabel yAxisLabel dataset))
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset
    dataset, ^PlotOrientation orientation, ^Boolean legend, ^Boolean
    tooltips, ^Boolean urls]
   (ChartFactory/createXYStepAreaChart title xAxisLabel yAxisLabel dataset
                                  orientation legend tooltips urls)))

;;XYDataset
;;Creates a stepped XY plot with default settings.
(defn ^JFreeChart  createXYStepChart
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset dataset]
   (ChartFactory/createXYStepChart  title xAxisLabel yAxisLabel dataset))
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^XYDataset
    dataset, ^PlotOrientation orientation, ^Boolean legend, ^Boolean
    tooltips, ^Boolean urls]
   (ChartFactory/createXYStepAreaChart title xAxisLabel yAxisLabel dataset
                                       orientation legend tooltips urls)))

;;WaferMapDataset
;;Creates a wafer map chart.
(defn ^JFreeChart  createWaferMapChart
  ([^String title, ^WaferMapDataset dataset, ^PlotOrientation orientation,
    ^Boolean legend, ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createWaferMapChart title dataset orientation legend tooltips urls)))


;;WindDataset
;;Creates a wind plot with default settings.
(defn ^JFreeChart createWindDataset
  ([^String title, ^String xAxisLabel, ^String yAxisLabel, ^WindDataset dataset,
    ^Boolean legend, ^Boolean tooltips, ^Boolean urls]
   (ChartFactory/createWindChart title xAxisLabel yAxisLabel dataset legend tooltips urls)))

;;Theming
;;=======

;;Returns the current chart theme used by the factory.
(defn ^ChartTheme getChartTheme [])

;;Sets the current chart theme.
(defn setChartTheme [^ChartTheme theme])




