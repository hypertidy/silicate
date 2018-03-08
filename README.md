
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](http://badges.herokuapp.com/travis/hypertidy/silicate?branch=master&env=BUILD_NAME=trusty_release&label=linux)](https://travis-ci.org/hypertidy/silicate) [![Build Status](http://badges.herokuapp.com/travis/hypertidy/silicate?branch=master&env=BUILD_NAME=osx_release&label=osx)](https://travis-ci.org/hypertidy/silicate) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/silicate?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/silicate) [![Coverage Status](https://img.shields.io/codecov/c/github/hypertidy/silicate/master.svg)](https://codecov.io/github/hypertidy/silicate?branch=master)

Overview
========

The goal of silicate is to bridge formal data structure definitions with flexible analytical and visualization techniques.

We

-   provide a universal *common-form* of hierarchical data
-   provide a framework for a *universal converter* between complex data types
-   to work with topological primitives for analysis and interaction.

We have created the general model `SC` being a core representation. Further models `PATH`, `ARC`, and `TRI` cover a broad range of complex types, and each is fundamental and distinct from the others. `SC` can be used to represent any model, but other models provide a better match to specific use-cases, intermediate forms and serve to expand the relationships between the model types.

-   `SC` is the universal model, composed of binary relationships, edges defined by pairs of vertices (a structural primitive model)
-   `TRI` also a structural primitive model, for triangulations
-   `PATH` a sequential model, for the standard spatial vector types, shapes defined by *paths*
-   `ARC` a sequential model, for *arc-node topology* a shared-boundary decomposition of path models

An extension of the `TRI` model `DEL` is provided in [anglr](https://github.com/hypertidy/anglr/) which builds *high-quality* triangulations, but the structural representation is the same.

Each model is created by using a set of generic verbs that extract the underlying elements of a given model. This design means that the models themselves are completely generic, and methods for worker verbs can be defined as needed for a given context. Our ideal situation would be for external packages to publish methods for these verbs, keeping package-specific code in the original package. We think this provides a very powerful and general mechanism for a family of consistent packages.

We have the following worker verbs that are used to build the above models, and work between what each model offers.

-   `sc_object` - highest level properties, the "features"
-   `sc_coord` - all instances of coordinates, labelled by vertex if the source model includes them
-   `sc_vertex` - only unique coordinates (in some geometric space)
-   `sc_path` - individual paths, sequential traces
-   `sc_edge` - unique binary relations, unordered segments
-   `sc_segment` - all instances of edges
-   `sc_arc` - unique topological paths, arcs either meet two other arcs at a node, or include no nodes
-   `sc_node` - unique nodes
-   `unjoin` - a function to *un join* a table, the opposite of the database join

The `unjoin` is a bit out of place here, but it's a key step when building these models, used to remove duplication at various levels. It's the primary mechanism for *defining and building-in* topology, which is precisely the relationships between entities in a model. This function is published in the [CRAN package unjoin](https://CRAN.R-project.org/package=unjoin).

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
#>  1 0.    0.    6e9815cdc2
#>  2 0.    1.00  d0f5ea8084
#>  3 0.750 1.00  5c519a6b89
#>  4 1.00  0.800 3ddbadc733
#>  5 0.500 0.700 d2a3fb324c
#>  6 0.800 0.600 586e076b94
#>  7 0.690 0.    1c5fa7e53c
#>  8 0.200 0.200 4a93e38b25
#>  9 0.500 0.200 729b57f72c
#> 10 0.500 0.400 6ddcc2dbfa
#> 11 0.300 0.600 84ead36aaa
#> 12 0.200 0.400 4ed243e0c7
#> 13 1.10  0.630 39895e6f6c
#> 14 1.23  0.300 003d4598d3

sc_edge(x)
#> # A tibble: 15 x 3
#>    .vertex0   .vertex1   edge_     
#>    <chr>      <chr>      <chr>     
#>  1 6e9815cdc2 d0f5ea8084 38830b07da
#>  2 d0f5ea8084 5c519a6b89 b20efd630f
#>  3 5c519a6b89 3ddbadc733 c488b1c67e
#>  4 3ddbadc733 d2a3fb324c f916856f3e
#>  5 d2a3fb324c 586e076b94 81f712eb0f
#>  6 586e076b94 1c5fa7e53c 1bc6ce3c78
#>  7 1c5fa7e53c 6e9815cdc2 c9d3e75780
#>  8 4a93e38b25 729b57f72c ba6dc66bd0
#>  9 729b57f72c 6ddcc2dbfa 9fd01a93d0
#> 10 6ddcc2dbfa 84ead36aaa 49c3cf2c0d
#> 11 84ead36aaa 4ed243e0c7 3152beb605
#> 12 4ed243e0c7 4a93e38b25 77b7298ca9
#> 13 586e076b94 39895e6f6c 27b1e84591
#> 14 39895e6f6c 003d4598d3 73554dc47a
#> 15 003d4598d3 1c5fa7e53c 4486b0de62

sc_node(y)
#> # A tibble: 2 x 1
#>   vertex_   
#>   <chr>     
#> 1 5a8d2ef3cd
#> 2 6e4a1a08ad

sc_arc(y)
#> # A tibble: 4 x 2
#>   arc_       ncoords_
#>   <chr>         <int>
#> 1 1f9bfd54c1        2
#> 2 4ec2c66381        4
#> 3 612615e7af        6
#> 4 e2e0731cd9        7
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
