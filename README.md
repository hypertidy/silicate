
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/hypertidy/silicate.svg?branch=master)](https://travis-ci.org/hypertidy/silicate) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/silicate?branch=master&svg=true)](https://ci.appveyor.com/project/hypertidy/silicate) [![Coverage Status](https://img.shields.io/codecov/c/github/hypertidy/silicate/master.svg)](https://codecov.io/github/hypertidy/silicate?branch=master)

Overview
========

The goal of silicate is to provide a general common form for complex multi-dimensional data.

There are two main motivations for `silicate`:

-   provide a central common-form of structure data, and a "universal converter"
-   to work with topological primitives for analysis and interaction.

We have created the general models `SC`, `PATH`, `ARC`, and `TRI` to cover the broadest range of complex types, with `SC` being the core, universal representation. The other models exist to cover specific use-cases, intermediate forms or to illustrate the relationships between these model types.

Each model is created by using a set of generic verbs that extract the underlying elements of a given model. This design means that the models themselves are completely generic, and methods for worker verbs can be defined as needed for a given context. Our ideal situation would be for external packages to publish methods for these verbs, keeping package-specific code in the original package. We think this provides a very powerful and general mechanism for a family of consistent packages.

Silicate models are built by using lower level worker verbs, listed here.

We have the following worker verbs that are used to build the above models, and work between what each model offers.

-   `sc_object` - highest level properties, the "features"
-   `sc_coord` - all instances of coordinates, labelled by vertex if the source model includes them
-   `sc_vertex` - only unique coordinates (in some geometric space)
-   `sc_path` - individual paths, sequential traces
-   `sc_edge` - unique binary relations, unordered segments
-   `sc_segment` - all instances of edges
-   `sc_arc` - unique topological paths, arcs either meet two other arcs at a node, or include no nodes
-   `sc_node` - unique nodes

Installation
============

``` r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("hypertidy/silicate")
```

Usage
=====

Convert a known external model to a silicate model.

``` r
library(silicate)
x <- SC(minimal_mesh) ## convert simple features to universal form

y <- ARC(minimal_mesh) ## convert simple features to "arc-node" form
```

Obtain the elements of a known model type.

``` r
sc_vertex(x)
#> # A tibble: 14 x 3
#>       x_    y_ vertex_ 
#>    <dbl> <dbl> <chr>   
#>  1 0     0     bac0b6cb
#>  2 0     1.00  ee0764a0
#>  3 0.750 1.00  7b378197
#>  4 1.00  0.800 676b4eda
#>  5 0.500 0.700 3c2cc028
#>  6 0.800 0.600 d382e91b
#>  7 0.690 0     06bbbf74
#>  8 0.200 0.200 7bb4dfe9
#>  9 0.500 0.200 cab58b23
#> 10 0.500 0.400 51dc08f3
#> 11 0.300 0.600 a278603c
#> 12 0.200 0.400 4830f31e
#> 13 1.10  0.630 76877cc3
#> 14 1.23  0.300 5e9a0e79

sc_edge(x)
#> # A tibble: 15 x 3
#>    .vertex0 .vertex1 edge_   
#>    <chr>    <chr>    <chr>   
#>  1 bac0b6cb ee0764a0 e7050e3c
#>  2 ee0764a0 7b378197 d1dc156c
#>  3 7b378197 676b4eda 3be53067
#>  4 676b4eda 3c2cc028 51bdb11d
#>  5 3c2cc028 d382e91b 90b4e7af
#>  6 d382e91b 06bbbf74 b1c4214a
#>  7 06bbbf74 bac0b6cb e81545c4
#>  8 7bb4dfe9 cab58b23 80569ab5
#>  9 cab58b23 51dc08f3 ebe4b152
#> 10 51dc08f3 a278603c 02df18eb
#> 11 a278603c 4830f31e 625d2277
#> 12 4830f31e 7bb4dfe9 26c33e33
#> 13 d382e91b 76877cc3 fea9325c
#> 14 76877cc3 5e9a0e79 9e38b41d
#> 15 5e9a0e79 06bbbf74 26b60d4c

sc_node(y)
#> # A tibble: 2 x 1
#>   vertex_ 
#>   <chr>   
#> 1 45599a90
#> 2 cbe9b007

sc_arc(y)
#> # A tibble: 4 x 2
#>   arc_     ncoords_
#>   <chr>       <int>
#> 1 56cb61c6        7
#> 2 61a2ef33        4
#> 3 d24dbbbd        2
#> 4 f724daf9        6
```

silicate models
---------------

There are two kinds of models, *primitive* and *sequential*.

Primitive-based models are composed of *atomic* elements that may be worked with arbitrarily, by identity and grouping alone.

Sequential-based models are bound to ordering and contextual assumptions. We provide the `PATH` and `ARC` models as generic, relational forms that provide a convenient intermediate between external forms and primitives models. Further intermediate models exist, including monotone and convex decompositions of polygons.

There is one universal primitives-based model, an edge-only model with two tables at its core. Higher level structures are described by grouping tables, with as many levels as required. Any other model can be expressed in this form.

We also differentiate *structural primitives*, which are specializations that are more convenient or more efficient in certain cases. These include triangulations (2D primitives), and segment structures (1D primitives), and could provide higher dimensional forms (3D primitives, etc. ).

Currently, we provide support for the universal model `SC`, the sequential models `PATH` (simple features belongs here, amongst many others) and `ARC` (arc-node topology, TopoJSON-like, OpenStreetMap), and structural primitives `TRI`.

In practice a segment model is trivial to generate, "SEG" but we haven't done that. This would be analogous to the format used by `rgl::rgl.lines` or `spatstat::psp`.

We take care to allow for *labelling* (identity) of component elements, without taking full responsibility for maintaining them. Random IDs are created as needed, but any operation that works with existing IDs should be stable with them.

Context, and some related projects
----------------------------------

The key difference between the silicate approach and simple features is the separation of geometry and topology. This allows for normalization (de-duplication) of the entities that are present or that can be identitied. Simple features has no capacity to de-duplicate or otherwise identify vertices, edges, paths or arcs, though tools that work with simple features do construct these schemes routinely in order to perform operations. When these richer, topological structures are built they are usually then discarded and the vertices are again de-normalized and again expressed explicitly without recording any of the relationships. In this sense, simple features can be described as an *explicitly-stored PATH analogue*, and is no different from the model used by shapefiles, binary blobs in databases, and many other spatial vector formats. There are a number of notable exceptions to this including TopoJSON, Eonfusion, PostGIS, QGIS geometry generators, Fledermaus, Mapbox, WebGL, Threejs, D3, AFrame, Lavavu but unfortunately there's no overall scheme that can unify these richer structures.

The silicate family is composed of a small number of packages that apply the principles here, either to read from path forms or primitive forms. As work continues some of these will be incorporated into the silicate core, when that is possible without requiring heavy external dependencies.

-   [scgraph](https://github.com/hypertidy/scgraph)
-   [scspatstat](https://github.com/hypertidy/scspatstat)

scdb, sctrip, scrgl, scraster, scicosa,
