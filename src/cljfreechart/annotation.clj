(ns cljfreechart.annotation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ANNOTATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-pointer
  "
  Adds an arrow annotation to the given chart.

  Arguments:
    chart -- the chart to annotate
    x, y -- the coordinate to add the annotation


  Options:
      :text -- (default \"\") text to include at the end of the arrow
      :angle -- (default :nw) either a number indicating the angle of the arrow
                or a keyword indicating a direction (:north :nw :west :sw :south
                :se :east :ne)


  Examples:

    (use '(incanter core charts))
    (def x (range (* -2 Math/PI) (* 2 Math/PI) 0.01))
    (def plot (xy-plot x (sin x)))
    (view plot)
    ;; annotate the plot
    (doto plot
      (add-pointer (- Math/PI) (sin (- Math/PI)) :text \"(-pi, (sin -pi))\")
      (add-pointer Math/PI (sin Math/PI) :text \"(pi, (sin pi))\" :angle :ne)
      (add-pointer (* 1/2 Math/PI) (sin (* 1/2 Math/PI)) :text \"(pi/2, (sin pi/2))\" :angle :south))

    ;; try the different angle options
    (add-pointer plot 0 0 :text \"north\" :angle :north)
    (add-pointer plot 0 0 :text \"nw\" :angle :nw)
    (add-pointer plot 0 0 :text \"ne\" :angle :ne)
    (add-pointer plot 0 0 :text \"west\" :angle :west)
    (add-pointer plot 0 0 :text \"east\" :angle :east)
    (add-pointer plot 0 0 :text \"south\" :angle :south)
    (add-pointer plot 0 0 :text \"sw\" :angle :sw)
    (add-pointer plot 0 0 :text \"se\" :angle :se)


  "
  ([chart x y & options]
    (let [opts (when options (apply assoc {} options))
          text (or (:text opts) "")
          n (* -1/2 Math/PI)
          s (* 1/2 Math/PI)
          w Math/PI
          e 0
          nw (* -3/4 Math/PI)
          ne (* -1/4 Math/PI)
          se (* 1/4 Math/PI)
          sw (* 3/4 Math/PI)
          angle (if (nil? (:angle opts))
                    nw
                    (if (keyword? (:angle opts))
                      (cond
                        (= (:angle opts) :north) n
                        (= (:angle opts) :south) s
                        (= (:angle opts) :west) w
                        (= (:angle opts) :east) e
                        (= (:angle opts) :nw) nw
                        (= (:angle opts) :ne) ne
                        (= (:angle opts) :sw) sw
                        (= (:angle opts) :se) se)
                      (:angle opts)))
          anno (XYPointerAnnotation. text x y angle)]
      (do
        (cond
          (= angle n)
            (.setTextAnchor anno TextAnchor/BASELINE_CENTER)
          (= angle s)
            (.setTextAnchor anno TextAnchor/CENTER)
          (= angle w)
            (.setTextAnchor anno TextAnchor/CENTER_RIGHT)
          (= angle e)
            (.setTextAnchor anno TextAnchor/CENTER_LEFT)
          (= angle nw)
            (.setTextAnchor anno TextAnchor/CENTER_RIGHT)
          (= angle ne)
            (.setTextAnchor anno TextAnchor/CENTER_LEFT)
          (= angle sw)
            (.setTextAnchor anno TextAnchor/CENTER_RIGHT)
          (= angle se)
            (.setTextAnchor anno TextAnchor/CENTER_LEFT))
        (.addAnnotation (.getPlot chart) anno))
      chart)))


(defn add-text
  "
  Adds a text annotation centered at the given coordinates.

  Arguments:
    chart -- the chart to annotate
    x, y -- the coordinates to center the text
    text -- the text to add


  Examples:

    ;; PCA chart example
    (use '(incanter core stats charts datasets))
    ;; load the iris dataset
    (def iris (to-matrix (get-dataset :iris)))
    ;; run the pca
    (def pca (principal-components (sel iris :cols (range 4))))
    ;; extract the first two principal components
    (def pc1 (sel (:rotation pca) :cols 0))
    (def pc2 (sel (:rotation pca) :cols 1))

    ;; project the first four dimension of the iris data onto the first
    ;; two principal components
    (def x1 (mmult (sel iris :cols (range 4)) pc1))
    (def x2 (mmult (sel iris :cols (range 4)) pc2))

    ;; now plot the transformed data, coloring each species a different color
    (def plot (scatter-plot x1 x2
                            :group-by (sel iris :cols 4)
                            :x-label \"PC1\" :y-label \"PC2\" :title \"Iris PCA\"))
    (view plot)
    ;; add some text annotations
    (add-text plot -2.5 -6.5 \"Setosa\")
    (add-text plot -5 -5.5 \"Versicolor\")
    (add-text plot -8 -5.5 \"Virginica\")

  "
  ([chart x y text & options]
    (let [opts (when options (apply assoc {} options))
          anno (XYTextAnnotation. text x y)]
      (.addAnnotation (.getPlot chart) anno)
      chart)))


(defn add-polygon
  "
  Adds a polygon outline defined by a given coordinates. The last coordinate will
  close with the first. If only two points are given, it will plot a line.

  Arguments:
    chart -- the chart to add the polygon to.
    coords -- a list of coords (an n-by-2 matrix can also be used)


  Examples:
    (use '(incanter core stats charts))
    (def x (range -3 3 0.01))
    (def plot (xy-plot x (pdf-normal x)))
    (view plot)

    ;; add polygon to the chart
    (add-polygon plot [[-1.96 0] [1.96 0] [1.96 0.4] [-1.96 0.4]])
    ;; the coordinates can also be passed in a matrix
    ;; (def points (matrix [[-1.96 0] [1.96 0] [1.96 0.4] [-1.96 0.4]]))
    ;; (add-polygon plot points)
    ;; add a text annotation
    (add-text plot -1.25 0.35 \"95% Conf Interval\")

    ;; PCA chart example
    (use '(incanter core stats charts datasets))
    ;; load the iris dataset
    (def iris (to-matrix (get-dataset :iris)))
    ;; run the pca
    (def pca (principal-components (sel iris :cols (range 4))))
    ;; extract the first two principal components
    (def pc1 (sel (:rotation pca) :cols 0))
    (def pc2 (sel (:rotation pca) :cols 1))

    ;; project the first four dimension of the iris data onto the first
    ;; two principal components
    (def x1 (mmult (sel iris :cols (range 4)) pc1))
    (def x2 (mmult (sel iris :cols (range 4)) pc2))

    ;; now plot the transformed data, coloring each species a different color
    (def plot (scatter-plot x1 x2
                            :group-by (sel iris :cols 4)
                            :x-label \"PC1\" :y-label \"PC2\" :title \"Iris PCA\"))

    (view plot)
    ;; put box around the first group
    (add-polygon plot [[-3.2 -6.3] [-2 -6.3] [-2 -3.78] [-3.2 -3.78]])
    ;; add some text annotations
    (add-text plot -2.5 -6.5 \"Setosa\")
    (add-text plot -5 -5.5 \"Versicolor\")
    (add-text plot -8 -5.5 \"Virginica\")



  "
  ([chart coords & options]
    (let [opts (when options (apply assoc {} options))
          points (double-array (mapcat identity coords))
          anno (XYPolygonAnnotation. points)]
      (.addAnnotation (.getPlot chart) anno)
      chart)))

(defn add-image
  "
  Adds an image to the chart at the given coordinates.

  Arguments:
    chart -- the chart to add the polygon to.
    x, y -- the coordinates to place the image
    img -- a java.awt.Image object


  Examples:
    (use '(incanter core charts latex))

     (doto (function-plot sin -10 10)
      (add-image 0 0 (latex \"\\\\frac{(a+b)^2} {(a-b)^2}\"))
      view)

  "
  ([chart x y img & options]
    (let [opts (when options (apply assoc {} options))
          anno (org.jfree.chart.annotations.XYImageAnnotation. x y img)]
      (.addAnnotation (.getPlot chart) anno)
      chart)))
