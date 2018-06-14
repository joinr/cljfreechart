(ns cljfreechart.plot
  (:import [org.jfree.chart.plot Plot XYPlot CategoryPlot]))

;;One thing that becomes immediately clear:
;;JFreeChart is a pretty massive API.
;;Each of the plot classes has a cubic ton of
;;methods and fields to define / customize / update
;;behavior of a plot.

;;The task here is to tease out a minimal
;;wrapper for plots.
;;From there, we want to define 90% use-case
;;transformations, means of combination, etc.
;;That allow us to modify wrapped or proxied
;;chart object.

;;Ideally, we'd like to retain access to the
;;underlying JFreeChart objects, to
;;facilitate customization or other
;;concerns.
  
;;The typical structural idea in JFreeChart
;;that we have a hierarchy:
;;plot
;;  rangeaxes[0..n]
;;  domainaxes[0..n]
;;  dataset[0...n]
;;       series?[0..k]
;;  renderer[0..n]
;;  marks
;;  annotations
;;  titles


;;this isn't too far off from the notion of layers
;;in ggplot or other stuff...

;;So, one way to define general combinators is
;;to deal with plot as our highest strata...
;;we have operations on plots that globally
;;apply in some measure to child layers.

;;Then we have individual layers.
;;  A layer has a dataset and a renderer.
;;  maybe styling options and the like...

;;The plot has global options for visualization:
(def plot-methods '#{
getBackgroundAlpha
getBackgroundImage
getBackgroundImageAlignment
getBackgroundImageAlpha
getBackgroundPaint
getChart
getDatasetGroup
getDrawingSupplier
getForegroundAlpha
getInsets
getLegendItems
getNoDataMessage
getNoDataMessageFont
getNoDataMessagePaint
getOutlinePaint
getOutlineStroke
getParent
getPlotType
getRootPlot

;;Also there are global plot methods
;;these all return events that have a
;;getBlah method where Blah corresponds to
;;the type of change.....java ugh 
annotationChanged ;;getAnnotation  
axisChanged       ;;getAxis
datasetChanged    ;;getDataset
markerChanged     ;;getMarker

;;event-related
addChangeListener 
removeChangeListener
setNotify ;;turn on or off change delivery
notifyListeners ;;sends out plotchangeevent
handleClick ;;deals with some deeper stuff we may not care about atm.

;;rendering
clone
draw
drawBackground
drawBackgroundImage
drawOutline

;;queries
isNotify 
isOutlineVisible
resolveRangeAxisLocation
resolveDomainAxisLocation
fetchElementHintingFlag

;;aux
zoom
equals

;;fields
DEFAULT_OUTLINE_PAINT DEFAULT_LEGEND_ITEM_BOX DEFAULT_BACKGROUND_PAINT
DEFAULT_BACKGROUND_ALPHA DEFAULT_OUTLINE_STROKE DEFAULT_INSETS
DEFAULT_FOREGROUND_ALPHA DEFAULT_LEGEND_ITEM_CIRCLE
MINIMUM_HEIGHT_TO_DRAW MINIMUM_WIDTH_TO_DRAW ZERO
})

;;So plots kind of act like nodes in piccolo.
;;They define a rendering hierarchy and serve as a
;;coordination point for communicating changes.

;;At this strata, our primitives are going to be
;;layers - combinations of datasets and renderers.

;;So, we can add a layer to a chart...
;;where layer is defined as - at least -
;;a pair of rendering and dataset.

;;The question then becomes how to define and
;;modify layers...

;;We'd also like the ability to query existing layers,
;;if they're named.

;;Currently, series have a SeriesKey datum assigned.
;;datasets do not (relying on position arguments).


;;XYPlot introduces a shitload of additional methods...

(def xymethods 
'#{DEFAULT_CROSSHAIR_PAINT
 DEFAULT_CROSSHAIR_STROKE
 DEFAULT_CROSSHAIR_VISIBLE
 DEFAULT_GRIDLINE_PAINT
 DEFAULT_GRIDLINE_STROKE
 addAnnotation
 addDomainMarker
 addRangeMarker
 clearAnnotations
 clearDomainAxes
 clearDomainMarkers
 clearRangeAxes
 clearRangeMarkers
 configureDomainAxes
 configureRangeAxes
 drawAnnotations
 drawDomainTickBands
 drawRangeTickBands
 getAnnotations
 getAxisOffset
 getDataRange
 getDataset
 getDatasetCount
 getDatasetRenderingOrder
 getDomainAxis
 getDomainAxisCount
 getDomainAxisEdge
 getDomainAxisForDataset
 getDomainAxisIndex
 getDomainAxisLocation
 getDomainCrosshairPaint
 getDomainCrosshairStroke
 getDomainCrosshairValue
 getDomainGridlinePaint
 getDomainGridlineStroke
 getDomainMarkers
 getDomainMinorGridlinePaint
 getDomainMinorGridlineStroke
 getDomainTickBandPaint
 getDomainZeroBaselinePaint
 getDomainZeroBaselineStroke
 getFixedDomainAxisSpace
 getFixedLegendItems
 getFixedRangeAxisSpace
 getIndexOf
 getOrientation
 getQuadrantOrigin
 getQuadrantPaint
 getRangeAxis
 getRangeAxisCount
 getRangeAxisEdge
 getRangeAxisForDataset
 getRangeAxisIndex
 getRangeAxisLocation
 getRangeCrosshairPaint
 getRangeCrosshairStroke
 getRangeCrosshairValue
 getRangeGridlinePaint
 getRangeGridlineStroke
 getRangeMarkers
 getRangeMinorGridlinePaint
 getRangeMinorGridlineStroke
 getRangeTickBandPaint
 getRangeZeroBaselinePaint
 getRangeZeroBaselineStroke
 getRenderer
 getRendererCount
 getRendererForDataset
 getSeriesCount
 getSeriesRenderingOrder
 getShadowGenerator
 getWeight
 indexOf
 isDomainCrosshairLockedOnData
 isDomainCrosshairVisible
 isDomainGridlinesVisible
 isDomainMinorGridlinesVisible
 isDomainPannable
 isDomainZeroBaselineVisible
 isDomainZoomable
 isRangeCrosshairLockedOnData
 isRangeCrosshairVisible
 isRangeGridlinesVisible
 isRangeMinorGridlinesVisible
 isRangePannable
 isRangeZeroBaselineVisible
 isRangeZoomable
 mapDatasetToDomainAxes
 mapDatasetToDomainAxis
 mapDatasetToRangeAxes
 mapDatasetToRangeAxis
 org.jfree.chart.plot.XYPlot
 panDomainAxes panRangeAxes
 removeAnnotation
 removeDomainMarker
 removeRangeMarker
 render
 rendererChanged
 setAxisOffset
 setDataset
 setDatasetRenderingOrder
 setDomainAxes
 setDomainAxis
 setDomainAxisLocation
 setDomainCrosshairLockedOnData
 setDomainCrosshairPaint
 setDomainCrosshairStroke
 setDomainCrosshairValue
 setDomainCrosshairVisible
 setDomainGridlinePaint
 setDomainGridlineStroke
 setDomainGridlinesVisible
 setDomainMinorGridlinePaint
 setDomainMinorGridlineStroke
 setDomainMinorGridlinesVisible
 setDomainPannable
 setDomainTickBandPaint
 setDomainZeroBaselinePaint
 setDomainZeroBaselineStroke
 setDomainZeroBaselineVisible
 setFixedDomainAxisSpace
 setFixedLegendItems
 setFixedRangeAxisSpace
 setOrientation
 setQuadrantOrigin
 setQuadrantPaint
 setRangeAxes
 setRangeAxis
 setRangeAxisLocation
 setRangeCrosshairLockedOnData
 setRangeCrosshairPaint
 setRangeCrosshairStroke
 setRangeCrosshairValue
 setRangeCrosshairVisible
 setRangeGridlinePaint
 setRangeGridlineStroke
 setRangeGridlinesVisible
 setRangeMinorGridlinePaint
 setRangeMinorGridlineStroke
 setRangeMinorGridlinesVisible
 setRangePannable
 setRangeTickBandPaint
 setRangeZeroBaselinePaint
 setRangeZeroBaselineStroke
 setRangeZeroBaselineVisible
 setRenderer
 setRenderers
 setSeriesRenderingOrder
 setShadowGenerator
 setWeight
 zoomDomainAxes
 zoomRangeAxes})



