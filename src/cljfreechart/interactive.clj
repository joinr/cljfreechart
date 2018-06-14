(ns cljfreechart.interactive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIDER CONTROLS AND PLOTS
(defn get-series
  "get-series"
  ([chart]
     (-> chart .getPlot .getDataset .getSeries))
  ([chart series-idx]
     (first (seq (-> chart
                     .getPlot
                     (.getDataset series-idx)
                     .getSeries)))))

(defmethod set-data org.jfree.chart.JFreeChart
  ([chart data]
     (set-data chart data 0))
  ([chart data series-idx]
     (let [series (get-series chart series-idx)]
       (.clear series)
       (cond
         (= 2 (count (first data)))
           (doseq [row data]
             (.addOrUpdate series (first row) (second row)))
         (= 2 (count data))
           (dorun (map #(.addOrUpdate series %1 %2) (first data) (second data)))
         :else
           (throw (Exception. "Data has wrong number of dimensions")))
       chart))
  ; by using (data-as-list) these two signatures do not only work for two
  ; column names and a dataset, but also when x and y are collections, in
  ; which case the dataset is ignored
  ([chart x y dataset]
    (set-data chart x y dataset 0))
  ([chart x y dataset series-idx]
     (let [series (get-series chart series-idx)]
       (.clear series)
       (dorun (map #(.addOrUpdate series %1 %2) (data-as-list x dataset) (data-as-list y dataset)))
       chart)))



(defn slider
  "
  Examples:
    (use '(incanter core stats charts))

    (def pdf-chart (function-plot pdf-normal -3 3))
    (view pdf-chart)
    (add-function pdf-chart pdf-normal -3 3)

    (let [x (range -3 3 0.1)]
      (slider #(set-data pdf-chart [x (pdf-normal x :sd %)]) (range 0.1 10 0.1)))

    (let [x (range -3 3 0.1)]
      (slider #(set-data pdf-chart [x (pdf-normal x :sd %)]) (range 0.1 10 0.1) \"sd\"))
  "
  ([updater-fn slider-values]
     (slider updater-fn slider-values nil))
  ([updater-fn slider-values slider-label]
     (let [max-idx (dec (count slider-values))
           label-txt (fn [v] (str (when slider-label (str slider-label " = ")) v))
           label (JLabel. (label-txt (first slider-values)) JLabel/CENTER)
           slider (doto (JSlider. JSlider/HORIZONTAL 0 max-idx 0)
                    (.addChangeListener (proxy [javax.swing.event.ChangeListener] []
                                          (stateChanged [^javax.swing.event.ChangeEvent event]
                                                        (let [source (.getSource event)
                                                              value (nth slider-values (.getValue source))]
                                                          (do
                                                            (.setText label (label-txt value))
                                                            (updater-fn value)))))))
           panel (doto (JPanel. (BorderLayout.))
                   (.add label BorderLayout/NORTH)
                   (.add slider BorderLayout/CENTER))
           frame (JFrame. "Slider Control")
           width 500
           height 70]
       (doto frame
         (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
         (.add panel BorderLayout/CENTER)
         (.setSize width height)
         ;; (.setVisible true)
         (.setVisible true))
       frame)))



(defn sliders*
  "sliders*

  Examples:
    (use '(incanter core stats charts))

    (let [x (range -3 3 0.1)]
      (do
        (def pdf-chart (xy-plot x (pdf-normal x :mean -3 :sd 0.1)))
        (view pdf-chart)
        (sliders* #(set-data pdf-chart [x (pdf-normal x :mean %1 :sd %2)])
                 [(range -3 3 0.1) (range 0.1 10 0.1)]
                 [\"mean\" \"sd\"])))
  "
  ([f [& slider-values]]
     (sliders* f (apply vector slider-values) [nil]))
  ([f [& slider-values] [& slider-labels]]
     (let [init-values (map first slider-values)
           refs (map ref init-values)
           slider-fns (map #(fn [v]
                              (do
                                (dosync (ref-set (nth refs %) v))
                                (apply f (map deref refs))))
                           (range (count refs)))
           _ ((first slider-fns) (first init-values))]
       (if slider-labels
         (map slider slider-fns slider-values slider-labels)
         (map slider slider-fns slider-values)))))


(defmacro sliders
  "
  Creates one slider control for each of the given sequence bindings.
  Each slider calls the given expression when manipulated.


  Examples:
    (use '(incanter core stats charts))

    ;; manipulate a normal pdf
    (let [x (range -3 3 0.1)]
      (def pdf-chart (xy-plot))
      (view pdf-chart)
      (sliders [mean (range -3 3 0.1)
                stdev (range 0.1 10 0.1)]
        (set-data pdf-chart [x (pdf-normal x :mean mean :sd stdev)])))


    ;; manipulate a gamma pdf
    (let [x (range 0 20 0.1)]
      (def pdf-chart (xy-plot))
      (view pdf-chart)
      (sliders [scale (range 0.1 10 0.1)
                shape (range 0.1 10 0.1)]
               (set-data pdf-chart [x (pdf-gamma x :scale scale :shape shape)])))



    ;; find the start values of a non-linear model function
    (use '(incanter core charts datasets))
    ;; create model function used in the following data-sorcery post:
    ;; http://data-sorcery.org/2009/06/06/fitting-non-linear-models/

    (defn f [theta x]
      (let [[b1 b2 b3] theta]
        (div (exp (mult (minus b1) x)) (plus b2 (mult b3 x)))))

    (with-data (get-dataset :chwirut)
      (view $data)
      (def chart (scatter-plot ($ :x) ($ :y)))
      (view chart)
      (add-lines chart ($ :x) (f [0 0.01 0] ($ :x)))

      ;; manipulate the model line to find some good start values.
      ;; give the index of the line data (i.e. 1) to set-data.
      (let [x ($ :x)]
        (sliders [b1 (range 0 2 0.01)
                  b2 (range 0.01 2 0.01)
                  b3 (range 0 2 0.01)]
          (set-data chart [x (f [b1 b2 b3] x)] 1))))

  "
  ([[& slider-bindings] body]
     `(let [slider-fn# (fn ~(apply vector (map symbol (take-nth 2 slider-bindings)))
                         (do ~body))
            slider-labels# ~(apply vector (map str (take-nth 2 slider-bindings)))]
        (sliders* slider-fn# ~(apply vector (take-nth 2 (rest slider-bindings))) slider-labels#))))




(defmacro dynamic-xy-plot
  "
  Returns an xy-plot bound to sliders (which tend to appear behind the chart).
  See the sliders macro for more information.


  Examples:

    (use '(incanter core stats charts))

    (let [x (range -3 3 0.1)]
    (view (dynamic-xy-plot [mean (range -3 3 0.1)
                            sd (range 0.1 10 0.1)]
                           [x (pdf-normal x :mean mean :sd sd)]
                           :title \"Normal PDF Plot\")))

   (let [x (range -3 3 0.1)]
     (view (dynamic-xy-plot [mean (range -3 3 0.1)
                             sd (range 0.1 10 0.1)]
            (for [xi x] [xi (pdf-normal xi :mean mean :sd sd)])
            :title \"Normal PDF Plot\")))


  "
  ([[& slider-bindings] expression & options]
     `(let [chart# (xy-plot [] [] ~@options)
            sliders# (sliders ~(vec slider-bindings)
                              (set-data chart# ~expression))]
        (doall sliders#)
        (set-x-label chart# (str '~(first expression)))
        (set-y-label chart# (str '~(second expression))))))


(defmacro dynamic-scatter-plot
  "
  Returns an scatter-plot bound to sliders (which tend to appear behind the chart).
  See the sliders macro for more information.


  Examples:

  (use '(incanter core stats charts))

  (let [x (range -3 3 0.1)]
    (view (dynamic-scatter-plot [mean (range -3 3 0.1)
                                 sd (range 0.1 10 0.1)]
            [x (pdf-normal x :mean mean :sd sd)]
            :title \"Normal PDF Plot\")))


   (let [x (range -3 3 0.1)]
     (view (dynamic-scatter-plot [mean (range -3 3 0.1)
                                  sd (range 0.1 10 0.1)]
            (for [xi x] [xi (pdf-normal xi :mean mean :sd sd)])
            :title \"Normal PDF Plot\")))

  "
  ([[& slider-bindings] expression & options]
     `(let [chart# (scatter-plot [] [] ~@options)
            sliders# (sliders ~(vec slider-bindings)
                              (set-data chart# ~expression))]
        (doall sliders#)
        (set-x-label chart# (str '~(first expression)))
        (set-y-label chart# (str '~(second expression))))))
