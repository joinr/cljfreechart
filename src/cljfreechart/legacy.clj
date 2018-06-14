(ns cljfreechart.legacy
  (:require  [clojure.spec.alpha :as s]
             [clojure.spec.gen.alpha :as gen]
             [spec-tools.data-spec :as ds]))

;;this is an example for building a
;;blended box-and-whisker and xyline chart.

;;You can see the breakdown...
;; private void display() {
;;         JFrame f = new JFrame("Test");
;;         f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);


;;Define the category datasets for Boxes
;;         DefaultBoxAndWhiskerCategoryDataset boxData = new DefaultBoxAndWhiskerCategoryDataset();
;;Create two categories in a series "Planet"
;;         boxData.add(Arrays.asList(30, 36, 46, 55, 65, 76, 81, 80, 71, 59, 44, 34), "Planet", "Endor");
;;         boxData.add(Arrays.asList(22, 25, 34, 44, 54, 63, 69, 67, 59, 48, 38, 28), "Planet", "Hoth");
;;Define a visual representation (renderer)
;;         BoxAndWhiskerRenderer boxRenderer = new BoxAndWhiskerRenderer();
;;Define another overlaid category dataset
;;         DefaultCategoryDataset catData = new DefaultCategoryDataset();

;;Define a series called "Mean", adding two categories (column 0, column 1)
;;this is basically looking up the column names from the boxdata...
;;         catData.addValue(boxData.getMeanValue(0, 0), "Mean", boxData.getColumnKey(0));
;;         catData.addValue(boxData.getMeanValue(0, 1), "Mean", boxData.getColumnKey(1));
;;Define a renderer for the new dataset
;;         LineAndShapeRenderer lineRenderer = new LineAndShapeRenderer();
;;Define axes
;;x -> category axis labeled "Type"
;;         CategoryAxis xAxis = new CategoryAxis("Type");
;;y -> numeric axis labeled "Value"
;;         NumberAxis yAxis = new NumberAxis("Value");
;;Alter properties on y, don't include 0 in autorange.
;;         yAxis.setAutoRangeIncludesZero(false);

;;Define a new CategoryPlot (NOT xy)
;;Note: all the matters initiallay is the
;;         CategoryPlot plot = new CategoryPlot(boxData, xAxis, yAxis, boxRenderer);
;;         plot.setDataset(1, catData);
;;         plot.setRenderer(1, lineRenderer);
;;         plot.setDatasetRenderingOrder(DatasetRenderingOrder.FORWARD);
;;         JFreeChart chart = new JFreeChart("Test", JFreeChart.DEFAULT_TITLE_FONT, plot, true);
;;         f.add(new ChartPanel(chart){
;;             @Override
;;             public Dimension getPreferredSize() {
;;                 return new Dimension(320, 480);
;;             }
;;         });
;;         f.pack();
;;         f.setLocationRelativeTo(null);
;;         f.setVisible(true);
;;     }




;;borrowing from altair, we can understand / translate the above chart....

;;the data representation and visuals are intertwined...
;;a mark, like mark_bar, ends up implying a dataset receptable and a renderer.

;; Mark Name 	Method 	Description 	Example
;; area 	mark_area() 	A filled area plot. 	Simple Stacked Area Chart
;; bar 	mark_bar() 	A bar plot. 	Simple Bar Chart
;; circle 	mark_circle() 	A scatter plot with filled circles. 	One Dot Per Zipcode
;; geoshape 	mark_geoshape() 	A geographic shape 	Choropleth Map
;; line 	mark_line() 	A line plot. 	Simple Line Chart
;; point 	mark_point() 	A scatter plot with configurable point shapes. 	Faceted Scatter Plot with Linked Brushing
;; rect 	mark_rect() 	A filled rectangle, used for heatmaps 	Simple Heatmap
;; rule 	mark_rule() 	A vertical or horizontal line spanning the axis. 	Candlestick Chart
;; square 	mark_square() 	A scatter plot with filled squares. 	N/A
;; text 	mark_text() 	A scatter plot with points represented by text. 	Simple Bar Chart with Labels
;; tick 	mark_tick() 	A vertical or horizontal tick mark. 	Strip Plot


;;reversing the build order
;;we have data.
(def data 
  {"Planet" {"Endor" [30, 36, 46, 55, 65, 76, 81, 80, 71, 59, 44, 34]
             "Hoth"  [22, 25, 34, 44, 54, 63, 69, 67, 59, 48, 38, 28]}})

;;Create two categories in a series "Planet"
;;         boxData.add(Arrays.asList(30, 36, 46, 55, 65, 76, 81, 80, 71, 59, 44, 34), "Planet", "Endor");
;;         boxData.add(Arrays.asList(22, 25, 34, 44, 54, 63, 69, 67, 59, 48, 38, 28), "Planet", "Hoth");

;;so, projections or encodings take data and plot them onto visual channels.
;;visual channels are things like axes.

;;so, the dataset (platform) isn't built until later.

;;we need to keep track of a couple of bits of information.
;;We'll make this more robust later....

;;basic encoding...
;;Position Channels
;; Channel 	Altair Class 	Description 	Example
;; x 	X 	The x-axis value 	Simple Scatter Plot
;; y 	Y 	The y-axis value 	Simple Scatter Plot
;; x2 	X2 	Second x value for ranges 	Error Bars showing Confidence Interval
;; y2 	Y2 	Second y value for ranges 	Line chart with Confidence Interval Band
;; longitude 	Longitude 	Longitude for geo charts 	Locations of US Airports
;; latitude 	Latitude 	Latitude for geo charts 	Locations of US Airports
;; longitude2 	Longitude2 	Second longitude value for ranges 	N/A
;; latitude2 	Latitude2 	Second latitude value for ranges 	N/A

(def position
  #{:x :y :x2 :y2 :longitude :latitude :longitude2 :latitude2})

(def position
  {;::id integer?
   ;::age ::age
   ;:boss boolean?
   ;(ds/req :name) string?
   ;(ds/opt :description) string?
   ;:languages #{keyword?}
   ;; :aliases [(ds/or {:maps {:alias string?}
   ;;                   :strings string?})]
   ;; :orders [{:id int?
   ;;           :description string?}]
   ;; :address (ds/maybe
   ;;            {:street string?
   ;;             :zip string?})
   :x   string? 
   :y   string?
   :x2  string?
   :y2  string?
   :longitude string?
   :latitude  string?
   :latitude2 string?
   })

(def position-spec (ds/spec ::position position))

;(defmap ->position [x y x2 y2 color shape longitude latitude longitude2 latitude2])

;; Mark Property Channels:
;; Channel 	Altair Class 	Description 	Example
;; color 	Color 	The color of the mark 	Simple Heatmap
;; fill 	Fill 	The fill for the mark 	N/A
;; opacity 	Opacity 	The opacity of the mark 	Horizon Graph
;; shape 	Shape 	The shape of the mark 	N/A
;; size 	Size 	The size of the mark 	Table Bubble Plot (Github Punch Card)
;; stroke 	Stroke 	The stroke of the mark 	N/A

(def mark
  #{:color :full :opacity :shape :size :stroke})

(def mark
  {:color   ::color
   :fill    double?
   :opacity 
   :shape
   :size
   :stroke
   })

(defn ->mark [color fill opacity shape size stroke])

;; Text and Tooltip Channels:
;; Channel 	Altair Class 	Description 	Example
;; text 	Text 	Text to use for the mark 	Simple Scatter Plot with Labels
;; key 	Key 	.. 	N/A
;; tooltip 	Tooltip 	The tooltip value 	N/A

(def text #{:text :key :tooltip})
;; Hyperlink Channel:
;; Channel 	Altair Class 	Description 	Example
;; href 	Href 	Hyperlink for points 	N/A
(def hyperlink #{:href})

;; Level of Detail Channel:
;; Channel 	Altair Class 	Description 	Example
;; detail 	Detail 	Additional property to group by 	Selection Detail Example
(def detail #{:detail})

;; Order Channel:
;; Channel 	Altair Class 	Description 	Example
;; order 	Order 	Sets the order of the marks 	Connected Scatterplot (Lines with Custom Paths)
(def order #{:order})

;; Facet Channels:
;; Channel 	Altair Class 	Description 	Example
;; column 	Column 	The column of a faceted plot 	Trellis Scatter Plot
;; row 	Row 	The row of a faceted plot 	Becker's Barley Trellis Plot
(def facet #{:column :row})

(defn ->chart [data marks encoding transforms]
  {:data       data
   :marks      marks
   :encoding   encoding
   :transforms transforms})

;;what if it's xy?
(defn ->box-marks
  ([ds xs series category]
   {:data (append-data ds xs series category)
    :marks :box}))

;;since we have a box-and-whisker mark, we define the boxplot
;;How do we know it's a category?
;;Typically color....

;;so, setting the mark for a plot sets up a bunch automatically...


;;Define the category datasets for Boxes
;;         DefaultBoxAndWhiskerCategoryDataset boxData = new DefaultBoxAndWhiskerCategoryDataset();




;;Define a visual representation (renderer)
;;         BoxAndWhiskerRenderer boxRenderer = new BoxAndWhiskerRenderer();

;;Define another overlaid category dataset
;;         DefaultCategoryDataset catData = new DefaultCategoryDataset();
