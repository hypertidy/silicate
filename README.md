
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/hypertidy/silicate.svg?branch=master)](https://travis-ci.org/hypertidy/silicate) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/silicate?branch=master&svg=true)](https://ci.appveyor.com/project/hypertidy/silicate) [![Coverage Status](https://img.shields.io/codecov/c/github/hypertidy/silicate/master.svg)](https://codecov.io/github/hypertidy/silicate?branch=master)

Overview
========

The goal of silicate is to provide a general common form for complex multi-dimensional data.

There are two main motivations for `silicate`:

-   provide a central common-form of structure data, and a "universal converter"
-   to work with topological primitives for analysis and interaction.

We have created three models `SC`, `PATH`, `ARC`, and `TRI` which cover the broadest range of complex types, with `SC` being the core, universal representation. Other models exist to cover specific use-cases or specific intermediate forms.

Each model is created by using a set of generic verbs that extract the underlying elements of a given model. This design means that the models themselves are completely generic, and methods for worker verbs can be defined as needed for a given context.

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
#>  1 0     0     7abca987
#>  2 0     1.00  e05fff09
#>  3 0.750 1.00  35b7660c
#>  4 1.00  0.800 e4e25979
#>  5 0.500 0.700 7ddb2ba8
#>  6 0.800 0.600 c693e3b6
#>  7 0.690 0     98aa80f1
#>  8 0.200 0.200 166ea6e4
#>  9 0.500 0.200 8da0f41c
#> 10 0.500 0.400 26e8183d
#> 11 0.300 0.600 b7937c49
#> 12 0.200 0.400 02606d61
#> 13 1.10  0.630 6abc7d7e
#> 14 1.23  0.300 b3a78626

sc_edge(x)
#> # A tibble: 15 x 3
#>    .vertex0 .vertex1 edge_   
#>    <chr>    <chr>    <chr>   
#>  1 7abca987 e05fff09 c11e2b8f
#>  2 e05fff09 35b7660c 23295efd
#>  3 35b7660c e4e25979 1b9494f5
#>  4 e4e25979 7ddb2ba8 fc3d5d5c
#>  5 7ddb2ba8 c693e3b6 0f09509a
#>  6 c693e3b6 98aa80f1 79b900a0
#>  7 98aa80f1 7abca987 2b787b6e
#>  8 166ea6e4 8da0f41c ab9bd8ee
#>  9 8da0f41c 26e8183d 18ad60c9
#> 10 26e8183d b7937c49 2c30f7bf
#> 11 b7937c49 02606d61 09d7d78d
#> 12 02606d61 166ea6e4 e771ed04
#> 13 c693e3b6 6abc7d7e 4900a828
#> 14 6abc7d7e b3a78626 cd525ba7
#> 15 b3a78626 98aa80f1 88bbd192

sc_node(y)
#> # A tibble: 2 x 1
#>   vertex_ 
#>   <chr>   
#> 1 42497d57
#> 2 9eac8751

sc_arc(y)
#> # A tibble: 4 x 2
#>   arc_     ncoords_
#>   <chr>       <int>
#> 1 3dbf295c        2
#> 2 3e7c43ec        4
#> 3 93b6904d        6
#> 4 f0b65048        7
```

silicate models
---------------

There are two kinds of models, *primitive* and *sequential*.

Primitive-based models are composed of *atomic* elements that may be worked with arbitrarily, by identity and grouping alone.

Sequential-based models are bound to ordering and contextual assumptions. We provide the `PATH` and `ARC` models as generic, relational forms that provide a convenient intermediate between external forms and primitives models. Further intermediate models exist, including monotone and convex decompositions of polygons.

There is one universal primitives-based model, an edge-only model with two tables at its core. Higher level structures are described by grouping tables, with as many levels as required. Any other model can be expressed in this form.

We also differentiate *structural primitives*, which are specializations that are more convenient or more efficient in certain cases. These include triangulations (2D primitives), and segment structures (1D primitives), and could provide higher dimensional forms (3D primitives, etc. ).

Currently, we provide support for the universal model `SC`, the sequential models `PATH` (simple features) and `ARC` (arc-node topology, TopoJSON-like), and structual primitives `TRI`.

In practice a segment model is trivial to generate, "SEG" but we haven't done that.

We take care to allow for *labelling* (identity) of component elements, without taking full responsibility for maintaining them. Random IDs are created as needed, but any operation that works with existing IDs should be stable with them.

Exceptions
----------

There are a number of notable exceptions in the spatial world, but unfortunately this highlights how fragemented the landscape is.

TopoJSON, Eonfusion, PostGIS, QGIS geometry generators, Fledermaus, ...

Other technologies are Mapbox, WebGL, Threejs, D3, AFrame, Lavavu ...

The silicate family is composed of a small number of packages that apply the principles here, either to read from path forms or primitive forms.

-   [scgraph](https://github.com/hypertidy/scgraph)
-   [scspatstat](https://github.com/hypertidy/scspatstat)

scdb, sctrip, scrgl, scraster, scicosa,
