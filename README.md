
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](http://badges.herokuapp.com/travis/hypertidy/silicate?branch=master&env=BUILD_NAME=trusty_release&label=linux)](https://travis-ci.org/hypertidy/silicate) [![Build Status](http://badges.herokuapp.com/travis/hypertidy/silicate?branch=master&env=BUILD_NAME=osx_release&label=osx)](https://travis-ci.org/hypertidy/silicate) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/silicate?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/silicate) [![Coverage Status](https://img.shields.io/codecov/c/github/hypertidy/silicate/master.svg)](https://codecov.io/github/hypertidy/silicate?branch=master)

Overview
========

The goal of silicate is to provide a general normal-form for complex multi-dimensional data.

These are the primary motivations for `silicate`:

-   provide a universal *common-form* of hierarchical data
-   provide a framework for a *universal converter* between complex data types
-   to work with topological primitives for analysis and interaction.

We have created the general models `SC`, `PATH`, `ARC`, and `TRI` to cover the broadest range of complex types, with `SC` being the core, universal representation. The other models exist to cover specific use-cases, intermediate forms or to illustrate the relationships between these model types.

-   `SC` the universal model, composed of binary relationships, edges defined by pairs of vertices
-   `PATH` a sequential model, for the standard spatial vector types, shapes defined by *paths*
-   `ARC` a sequential model, for *arc-node topology* a shared-boundary decomposition of path models
-   `TRI` a structural primitive model, for triangulations

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
#>  1 0.    0.    ee907f88ae
#>  2 0.    1.00  7a9513f101
#>  3 0.750 1.00  2796b5bd58
#>  4 1.00  0.800 f4fec84ab9
#>  5 0.500 0.700 124e33d89b
#>  6 0.800 0.600 b67882940c
#>  7 0.690 0.    5b1ab9cd41
#>  8 0.200 0.200 a89fc24da7
#>  9 0.500 0.200 41e1c69582
#> 10 0.500 0.400 f1cde42d5c
#> 11 0.300 0.600 4dc4d79de2
#> 12 0.200 0.400 bf1b334088
#> 13 1.10  0.630 380dddd669
#> 14 1.23  0.300 03b237aced

sc_edge(x)
#> # A tibble: 15 x 3
#>    .vertex0   .vertex1   edge_     
#>    <chr>      <chr>      <chr>     
#>  1 ee907f88ae 7a9513f101 026c7d076c
#>  2 7a9513f101 2796b5bd58 1ee399b09f
#>  3 2796b5bd58 f4fec84ab9 a25c5e05fd
#>  4 f4fec84ab9 124e33d89b 20a5c5fcb4
#>  5 124e33d89b b67882940c 4322441973
#>  6 b67882940c 5b1ab9cd41 1a6848c480
#>  7 5b1ab9cd41 ee907f88ae 13577bcf48
#>  8 a89fc24da7 41e1c69582 53e9fb0489
#>  9 41e1c69582 f1cde42d5c 0370b47d85
#> 10 f1cde42d5c 4dc4d79de2 65e2ee4eae
#> 11 4dc4d79de2 bf1b334088 a643290445
#> 12 bf1b334088 a89fc24da7 e1d56c9a2b
#> 13 b67882940c 380dddd669 453ca767b8
#> 14 380dddd669 03b237aced b7f9866720
#> 15 03b237aced 5b1ab9cd41 f66ee7b803

sc_node(y)
#> # A tibble: 2 x 1
#>   vertex_   
#>   <chr>     
#> 1 b84d684631
#> 2 6449d42eeb

sc_arc(y)
#> # A tibble: 4 x 2
#>   arc_       ncoords_
#>   <chr>         <int>
#> 1 1614004bd2        6
#> 2 3d36192a3c        4
#> 3 4b4f726fd3        7
#> 4 5784c80cf0        2
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
