(ns cljfreechart.charts.heat)

(def byr-gradient
  [[0 0 127]
   [0 0 212]
   [0 42 255]
   [0 127 255]
   [0 127 255]
   [0 226 255]
   [42 255 212]
   [56 255 198]
   [255 212 0]
   [255 198 0]
   [255 169 0]
   [255 112 0]
   [255 56 0]
   [255 14 0]
   [255 42 0]
   [226 0 0]])

(def rg-gradient
  [[229 0 18]
   [206 19 16]
   [184 38 14]
   [162 57 12]
   [140 76 10]
   [118 95 9]
   [95 114 7]
   [73 133 5]
   [29 171 1]
   [7 191 0]])

(def ryg-gradient
  [[229 0 17]
   [225 28 0]
   [221 73 0]
   [217 116 0]
   [213 157 0]
   [210 197 0]
   [175 206 0]
   [131 202 0]
   [88 198 0]
   [46 194 0]
   [7 191 0]])

;;defines a color scale gradient from [blue red] [min max]
(def +default-heat-palette+
  {:blue-yellow-red  byr-gradient
   :red-yellow-blue  (vec (reverse byr-gradient))
   :red-green        rg-gradient
   :green-red        (vec (reverse rg-gradient))
   :red-yellow-green ryg-gradient
   :green-yellow-red (vec (reverse ryg-gradient))})

(def default-heat-legend-options
  {:axis-location :bottom-or-left
   :subdivisions  20
   :axis-offset   5.0
   :frame         (org.jfree.chart.block.BlockBorder. java.awt.Color/red)
   :margin        (org.jfree.ui.RectangleInsets. 5 5 5 5)
   :padding       (org.jfree.ui.RectangleInsets. 10 10 10 10)
   :strip-width   10
   :position      :right})


(defn ^org.jfree.chart.title.PaintScaleLegend ->heat-legend
  "Defines a legend for a heatmap based on the PaintScaleLegend
   JFreeChart class."
  ([scale scale-axis legend-opts]
   (let [{:keys [axis-location
                 subdivisions
                 axis-offset
                 frame
                 margin
                 padding
                 strip-width
                 position]} legend-opts]
     (doto (org.jfree.chart.title.PaintScaleLegend. scale scale-axis)
       (.setSubdivisionCount subdivisions)
       (.setAxisLocation     (as-axis-location axis-location))
       (.setAxisOffset       axis-offset)
       (.setMargin           (org.jfree.ui.RectangleInsets. 5 5 5 5))
       ;(.setFrame            (org.jfree.chart.block.BlockBorder. java.awt.Color/red))
       (.setPadding          (org.jfree.ui.RectangleInsets. 10 10 10 10))
       (.setStripWidth       10)
       (.setPosition         (as-position position)))))
  ([scale scale-axis] (->heat-legend scale scale-axis default-heat-legend-options)))

(defn ->discrete-heat-legend
  ([items  options]
   (let [opts (merge default-heat-legend-options options) ;(apply assoc {} options)
         {:keys [frame
                 margin
                 padding
                 strip-width
                 position]}    opts]
     (doto (->discrete-legend items)
       (.setMargin           (org.jfree.ui.RectangleInsets. 5 5 5 5))
       ;(.setFrame            (org.jfree.chart.block.BlockBorder. java.awt.Color/red))
       (.setPadding          (org.jfree.ui.RectangleInsets. 10 10 10 10))
       ;(.setStripWidth       10)
       (.setPosition         (as-position position))))))

;;So, we want to construct a chart..
(defn heat-map*
  ([function x-min x-max y-min y-max & options]
     (let [opts   (when options (apply assoc {} options))
           color? (if (false? (:color? opts)) false true)
           include-zero? (if (false? (:include-zero? opts)) false true)
           title   (or (:title opts) "")
           x-label (or (:x-label opts) "")
           y-label (or (:y-label opts) "")
           z-label (or (:z-label opts) "z scale")
           x-res   (or (:x-res opts) 100)
           y-res   (or (:y-res opts) 100)
           auto-scale? (if (false? (:auto-scale? opts)) false true)
           grid-lines?  (:grid-lines? opts)
           discrete-legend? (:discrete-legend? opts)
           block-width  (double (/ (- x-max x-min) x-res))
           block-height (double (/ (- y-max y-min) y-res))
           theme (or (:theme opts) #(-> (set-theme % :default)
                                        (set-grid-lines grid-lines?)
                                        (black-grid-lines)))
           xyz-dataset (org.jfree.data.xy.DefaultXYZDataset.)
           data (into-array (map double-array
                                 (grid-apply function x-min x-max y-min y-max x-res y-res)))
           ;;TODO replace with ->number-axis....
           x-axis (doto (org.jfree.chart.axis.NumberAxis. x-label)
                    (.setStandardTickUnits (org.jfree.chart.axis.NumberAxis/createIntegerTickUnits))
                    (.setLowerMargin 0.0)
                    (.setUpperMargin 0.0)
                    (.setAxisLinePaint java.awt.Color/white)
                    (.setTickMarkPaint java.awt.Color/white)
                    (.setAutoRangeIncludesZero include-zero?))
           ;;TODO replace with ->number-axis....
           y-axis (doto (org.jfree.chart.axis.NumberAxis. y-label)
                    (.setStandardTickUnits (org.jfree.chart.axis.NumberAxis/createIntegerTickUnits))
                    (.setLowerMargin 0.0)
                    (.setUpperMargin 0.0)
                    (.setAxisLinePaint java.awt.Color/white)
                    (.setTickMarkPaint java.awt.Color/white)
                    (.setAutoRangeIncludesZero include-zero?))
           ;;all color scale stuff.
           colors   (or (:colors opts) (get +default-heat-palette+ :blue-yellow-red))
           colors   (if (keyword? colors) (or (get +default-heat-palette+ colors)
                                              (color/get-palette colors) 
                                              (throw (Exception. (str [:unknown-pallete colors]))))
                        colors)
           min-z    (or (:min-z opts) (reduce min (last data)))
           max-z    (or (:max-z opts) (reduce max (last data)))           
           scale       (derive-color-scale colors min-z max-z color?)
           scale-axis  (org.jfree.chart.axis.NumberAxis. z-label)
           color->item (into {} (map (fn [[threshold color :as c]]
                                       [c (if (map? color) color
                                              {:text  (str threshold)
                                               :color  color}
                                              )]) colors))                          
           legend     (if discrete-legend?
                        (->discrete-heat-legend (map color->item colors) (merge default-heat-legend-options                                                     
                                                                                opts
                                                                                (:legend-options opts)
                                                                                ))
                        (->heat-legend scale scale-axis
                                       (merge default-heat-legend-options                                                     
                                              opts
                                              (:legend-options opts)
                                            )))
           renderer   (org.jfree.chart.renderer.xy.XYBlockRenderer.)
           _       (when auto-scale?
                     (doto renderer
                       (.setBlockWidth block-width)
                       (.setBlockHeight block-height)))
           plot  (if grid-lines?
                   (->grided-xy-plot xyz-dataset x-axis y-axis renderer)
                   (org.jfree.chart.plot.XYPlot. xyz-dataset x-axis y-axis renderer))
           chart (org.jfree.chart.JFreeChart. plot)]
       (do
         (.setPaintScale renderer scale)
         (.addSeries xyz-dataset "Series 1" data)
         ;;should extract these into a theme.
         (.setBackgroundPaint plot java.awt.Color/lightGray)        
         (.setAxisOffset plot (org.jfree.ui.RectangleInsets. 5 5 5 5))
         (.setOutlinePaint plot java.awt.Color/blue)
         (.removeLegend chart) ;;eliminate default legend.
         (.setTitle chart title)
         (.addSubtitle chart legend)
         (org.jfree.chart.ChartUtilities/applyCurrentTheme chart)
         (set-theme chart theme)
         )
       chart)))

(defmacro heat-map
  "
  Usage: (heat-map function x-min x-max y-min y-max & options)

  Returns a JFreeChart object representing a heat map of the function across
  the given x and y ranges. Use the 'view' function to display the chart, or
  the 'save' function to write it to a file.  Callers may define the
  number of samples in each direction, and select if they want a
  sparser representation by disabling :auto-scale? .  By default,
  the heat-map will try to scale the 'blocks' or sampled pixels
  to cover the ranges specified.  Depending on the number of
  samples, this may result in a pixelated but performant look.
  Disabling auto-scale? will keep the 'blocks' a constant
  size, leading to potentially sparsely sampled points on
  the surface surrounded by blank regions.

  Arguments:
    function -- a function that takes two scalar arguments and returns a scalar
    x-min    -- lower bound for the first value of the function
    x-max    -- upper bound for the first value of the function
    y-min    -- lower bound for the second value of the function
    y-max    -- upper bound for the second value of the function

  Options:
    :title
    :x-label (default 'x-min < x < x-max')
    :y-label (default 'y-min < y < y-max')
    :z-label -- defaults to function's name
    :color? (default true) -- should the plot be in color or not?
    :include-zero? (default true) -- should the plot include the origin if it
                                     is not in the ranges specified?
    :x-res   (default 100) -- amount of samples to take in the x range
    :y-res   (default 100) -- amount of samples to take in the y range
    :auto-scale? (default true) -- automatically scale the block
                                   width/height to provide a continuous surface
    :discrete-legend? (default false) -- automatically subdivide the legend to match
                                         the discrete color scale vs. fixed subdivisions.
    :grid-lines? (:both)|:vertical|:horizontal -- renders grid-lines for the
                                                heat-map
    :min-z (defaults to minumum z from data) -- lower end of the color-scale
    :max-z (defaults to maximum z from data) -- upper end of the color-scale

  Examples:
    (use '(incanter core charts))
    (defn f [x y] (sin (sqrt (plus (sq x) (sq y)))))
    (view (heat-map f -10 10 -15 15))
    (view (heat-map f -10 10 -10 10 :color? false))
    (view (heat-map f 5 10 5 10 :include-zero? false))

    (defn f2 [x y] (plus (sq x) (sq y)))
    (view (heat-map f2 -10 10 -10 10))
    (view (heat-map f2 -10 10 -10 10 :color? false))

    (use 'incanter.stats)
    (defn f3 [x y] (pdf-normal (sqrt (plus (sq x) (sq y)))))
    (view (heat-map f3 -3 3 -3 3 :x-label \"x1\" :y-label \"x2\" :z-label \"pdf\"))
    (view (heat-map f3 -3 3 -3 3 :color? false))

    (defn f4 [x y] (minus (sq x) (sq y)))
    (view (heat-map f4 -10 10 -10 10))
    (view (heat-map f4 -10 10 -10 10 :color? false))


    (use '(incanter core stats charts))
    (let [data [[0 5 1 2]
                  [0 10 1.9 1]
                  [15 0 0.5 1.5]
                  [18 10 4.5 2.1]]
          diffusion (fn [x y]
                      (sum (map #(pdf-normal (euclidean-distance [x y] (take 2 %))
                                             :mean (nth % 2) :sd (last %))
                                data)))]
      (view (heat-map diffusion -5 20 -5 20)))

  "
  ([function x-min x-max y-min y-max & options]
    `(let [opts# ~(when options (apply assoc {} options))
           x-lab# (or (:x-label opts#) (format "%s < x < %s" '~x-min '~x-max))
           y-lab# (or (:y-label opts#) (format "%s < y < %s" '~y-min '~y-max))
           z-lab# (or (:z-label opts#) (str '~function))
           args# (concat [~function ~x-min ~x-max ~y-min ~y-max]
                         (apply concat (seq (apply assoc opts#
                                                   [:z-label z-lab#
                                                    :x-label x-lab#
                                                    :y-label y-lab#]))))]
       (apply heat-map* args#))))
