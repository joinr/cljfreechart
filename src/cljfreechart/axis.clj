(ns cljfreechart.axis)

(defn log-axis
  "
  Create a logarithmic axis.

  Note: By default, values smaller than 0.5 are rounded to 0.5 to prevent strange behavior that
  happens for values close to 0.

  Options:
    :base (default 10) base of the logarithm; typically 2 or 10
    :label (default none) the label of the axis
    :int-ticks? (default true) if true, use normal numbers instead of
       <base>^<exponent>, i.e. 1 instead of f.ex. 10^0.0
    :smallest-value (default: 0.5) Set the smallest value represented by the axis, set to 0.0 to 'reset'

  See also:
    set-axis

  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/axis/LogAxis.html

  "
  [& options]
  (let [opts (when options (apply assoc {} options))
        base (or (:base opts) 10)
        smallest-value (or (:smallest-value opts) 0.5)
        int-ticks? (get opts :int-ticks? true)
        label (:label opts)
        axis (if label
               (LogAxis. label)
               (LogAxis.))]
    (doto axis (.setBase base))
    (when int-ticks?
      (.setStandardTickUnits axis (NumberAxis/createIntegerTickUnits))) ;; must be after setting the base
    (when smallest-value
      (.setSmallestValue axis smallest-value)) ; TODO TEST THIS!
    axis))

(defmulti set-axis
  "
  Set the selected axis of the chart, returning the chart.
  (Beware: the axis' label will replace axis label set previously on the chart.)

  Arguments:
    chart - the JFreeChart object whose axis to change
    dimension - depends on the plot type for plots with mutliple axes
                 f.ex. :x or :y for an XYPlot (x is the domain axis, y the range one)
    axis - the axis to set, an instance of ValueAxis

  See also:
    log-axis

  Note:
    Not applicable to DialPlot MeterPlot PiePlot MultiplePiePlot CompassPlot WaferMapPlot SpiderWebPlot

  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/axis/ValueAxis.html
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/plot/XYPlot.html

  Examples:

    (use '(incanter core charts))

    (view
      (doto (function-plot #(Math/pow 10 %) 0 5)
            (set-axis :x (log-axis :base 10, :label \"log(x)\"))))
  "

  (fn [chart & args] (type (.getPlot chart))))

;;; Note: it would be nicer to use hierarchies to declare what plot types support
;;; the x and y axes but it feels as an overkill now
(defmethod set-axis :default
  ([chart axis] (throw (IllegalArgumentException. "Dimension (:x or :y) is required for XY-like charts")))
  ([chart dimension axis]
     {:pre [(#{:x :y} dimension)]}

     (let [plot (.getPlot chart)
           allowed-types #{org.jfree.chart.plot.XYPlot org.jfree.chart.plot.CategoryPlot org.jfree.chart.plot.ContourPlot org.jfree.chart.plot.FastScatterPlot}]
       (assert (allowed-types (type plot))
               (str "The default set-axis method only works for " allowed-types))
       (if (= :x dimension)
         (.setDomainAxis plot axis)
         (.setRangeAxis plot axis)))
     chart))

(defmethod set-axis org.jfree.chart.plot.PolarPlot
  ([chart axis]
     (let [plot (.getPlot chart)]
       (.setAxis plot axis))
     chart))

(defmethod set-axis org.jfree.chart.plot.ThermometerPlot
  ([chart axis]
     (let [plot (.getPlot chart)]
       (.setRangeAxis plot axis))
     chart))



(defn ^NumberAxis ->number-axis
  ([label] (org.jfree.chart.axis.NumberAxis. (str label)))
  ([label {:keys [units line-color tick-color include-zero?
                  lower-margin upper-margin]
           :or {lower-margin 0.0
                upper-margin 0.0
                line-color :white
                tick-color :white} :as opts}]
   (doto (org.jfree.chart.axis.NumberAxis. (str label))
     (.setStandardTickUnits (case units
                              :integer (org.jfree.chart.axis.NumberAxis/createIntegerTickUnits)
                              (org.jfree.chart.axis.NumberAxis/createStandardTickUnits)))
     (.setLowerMargin lower-margin)
     (.setUpperMargin upper-margin)
     (.setAxisLinePaint (chart-color line-color))
     (.setTickMarkPaint (chart-color tick-color))
     (.setAutoRangeIncludesZero include-zero?))))

#_(defn ^SymbolAxis ->symbol-axis [label symbols]
  (org.jfree.chart.axis.SymbolAxis. (str label)
                                    (into-array (map str symbols))))
