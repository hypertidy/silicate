
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
#>  1 0     0     6c9ce44a
#>  2 0     1.00  b97ac306
#>  3 0.750 1.00  7d5f0a8c
#>  4 1.00  0.800 09e2a4d3
#>  5 0.500 0.700 c5cbe873
#>  6 0.800 0.600 3a750d72
#>  7 0.690 0     8d9fd89a
#>  8 0.200 0.200 9654f8e5
#>  9 0.500 0.200 eae3e4f3
#> 10 0.500 0.400 d8d675be
#> 11 0.300 0.600 080dd0b5
#> 12 0.200 0.400 2d186c92
#> 13 1.10  0.630 5731c258
#> 14 1.23  0.300 5a0b399e

sc_edge(x)
#> # A tibble: 15 x 3
#>    .vertex0 .vertex1 edge_   
#>    <chr>    <chr>    <chr>   
#>  1 6c9ce44a b97ac306 1c14f0ef
#>  2 b97ac306 7d5f0a8c 36ec4b00
#>  3 7d5f0a8c 09e2a4d3 953c0779
#>  4 09e2a4d3 c5cbe873 3ad4950f
#>  5 c5cbe873 3a750d72 5dde4c66
#>  6 3a750d72 8d9fd89a 942dba53
#>  7 8d9fd89a 6c9ce44a 39e5fd1c
#>  8 9654f8e5 eae3e4f3 1bb1ff05
#>  9 eae3e4f3 d8d675be 63104ef6
#> 10 d8d675be 080dd0b5 a82ef968
#> 11 080dd0b5 2d186c92 c78390e3
#> 12 2d186c92 9654f8e5 5cebf8f9
#> 13 3a750d72 5731c258 120854df
#> 14 5731c258 5a0b399e 74634050
#> 15 5a0b399e 8d9fd89a 5d27c8b1

sc_node(y)
#> # A tibble: 2 x 1
#>   vertex_ 
#>   <chr>   
#> 1 dbfd8a5a
#> 2 84ca593c

sc_arc(y)
#> # A tibble: 4 x 2
#>   arc_     ncoords_
#>   <chr>       <int>
#> 1 0ad04dbd        2
#> 2 341cc58f        7
#> 3 ba9dbdc0        6
#> 4 c556ecca        4
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
