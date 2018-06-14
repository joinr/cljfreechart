# cljfreechart

Consolidated wrapper for JFreeChart 1.5+.

HEAVY WIP at the moment, unusable.
This is currently a refactoring of 
incanter.charts, along with several side 
projects and hacks to work around incanter's 
limitations.

Intent is to provide a pluggable backend
for incanter's charting API, with cljfreechart
forming the default.

cjfreechart itself will be inspired by incanter.charts,
but will not depend on incanter.core or anything else, 
but rather work with stock clojure data structures.
This should make it a decent rendering lib.

Other goals include providing a porcelain, 
or declarative rendering layer akin to ggplot or
vega (although not directly compatible).

The idea is to work declaratively in layers, marks, 
etc.

Another final goal is to enable some reactivity 
in the plotting, to allow modern abstractions to
provide dynamic plots.

Minimum target is feature parity with incanter.

I don't plan to have a complete wrapper for 
JFreeChart, but do plan to allow clients
to have unfettered access to the JFreeChart
objects if they need to deviate from what the
API provides, and touch on niche work arounds.

Hopefully, we hit the 90% use-case though.

## Usage

Don't use it yet! 
Heavily under construction

## License

Copyright © 2018 joinr

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
