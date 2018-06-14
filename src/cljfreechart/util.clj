(ns cljfreechart.util)

;;incanter specific....
(defn- data-as-list
  "
  data-as-list [x data]

  If x is a collection, return it
  If x is a single value, and data is undefined, return x in vector
  If x is a single value, and data is defined, return ($ x data)
  "
  [x data]
  (if (or (coll? x)
          (dataset? x)
          (matrix? x)
          (vec? x))
    (to-list x)
    (if data
      (let [selected ($ x data)]
        (if (coll? selected)
          selected
          [selected]))
      [x])))

(defn- in-coll
  "
  in-coll [x]

  If x is a collection, return it
  Otherwise return x in a vector"
  [x]
  (if (coll? x)
    x
    [x]))

(defn- range-inclusive
  "Similar to range but adds end to result."
  [start end step]
  (concat (range start end step) [end]))


;;JfreeChart defines a number of
;;backing datasets for plots.
(defmacro do! [expr]
  `(try ~expr 
        (catch ~'Exception ~'hidden nil)))

(defmacro find-method [m class] 
  `(.getDeclaredMethod ~class  ~(str m) (into-array ~'Class [])))

(defmacro invoke! [klass obj meth & args] 
  `(let [meth# (doto (find-method ~meth ~klass)
                 (.setAccessible true))]
     (.invoke meth# ~obj (into-array ~klass [~@args]))))

(defn falsey [v]
  (if (false? v) true false))

;;originally
;;used to help define discrete or continuous color scales.
(defn spread
  "Distribute a coll of xs across the numeric range [min max]
   by normalizing the count of xs relative the distance between
   max and min.  Caller can supply a function f to apply to the
   keys, otherwise defaults to rational division."
  ([min max f xs]
  (let [distance (- max min)
        xs       (vec    xs)
        len      (count  xs)
        step     (/ distance (dec len))]
    (map-indexed (fn [idx x]
                   [(f (+ (* idx step) min))
                    x]) xs)))
  ([min max xs] (spread min max identity xs)))

(defn spread-doubles
  "Like spread, with double values as indices."
  [min max xs]
  (spread min max double xs))

(defn spread-ints
  "Like spread, with integer values derived by
   truncating indices via long."
  [min max xs]
  (spread min max long xs))
