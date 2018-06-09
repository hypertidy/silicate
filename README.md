
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](http://badges.herokuapp.com/travis/hypertidy/silicate?branch=master&env=BUILD_NAME=trusty_release&label=linux)](https://travis-ci.org/hypertidy/silicate)
[![Build
Status](http://badges.herokuapp.com/travis/hypertidy/silicate?branch=master&env=BUILD_NAME=osx_release&label=osx)](https://travis-ci.org/hypertidy/silicate)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/silicate?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/silicate)
[![Coverage
Status](https://img.shields.io/codecov/c/github/hypertidy/silicate/master.svg)](https://codecov.io/github/hypertidy/silicate?branch=master)

# Overview

The goal of silicate is to bridge formal data structure definitions with
flexible analytical and visualization techniques.

We

  - provide a universal *common-form* of hierarchical data
  - provide a framework for a *universal converter* between complex data
    types
  - to work with topological primitives for analysis and interaction.

We have created the general model `SC` being a core representation.
Further models `PATH`, `ARC`, and `TRI` cover a broad range of complex
types, and each is fundamental and distinct from the others. `SC` can be
used to represent any model, but other models provide a better match to
specific use-cases, intermediate forms and serve to expand the
relationships between the model types.

  - `SC` is the universal model, composed of binary relationships, edges
    defined by pairs of vertices (a structural primitive model)
  - `TRI` also a structural primitive model, for triangulations
  - `PATH` a sequential model, for the standard spatial vector types,
    shapes defined by *paths*
  - `ARC` a sequential model, for *arc-node topology* a shared-boundary
    decomposition of path models

An extension of the `TRI` model `DEL` is provided in
[anglr](https://github.com/hypertidy/anglr/) which builds *high-quality*
triangulations, but the structural representation is the same.

Each model is created by using a set of generic verbs that extract the
underlying elements of a given model. This design means that the models
themselves are completely generic, and methods for worker verbs can be
defined as needed for a given context. Our ideal situation would be for
external packages to publish methods for these verbs, keeping
package-specific code in the original package. We think this provides a
very powerful and general mechanism for a family of consistent packages.

We have the following worker verbs that are used to build the above
models, and work between what each model offers.

  - `sc_object` - highest level properties, the “features”
  - `sc_coord` - all instances of coordinates, labelled by vertex if the
    source model includes them
  - `sc_vertex` - only unique coordinates (in some geometric space)
  - `sc_path` - individual paths, sequential traces
  - `sc_edge` - unique binary relations, unordered segments
  - `sc_segment` - all instances of edges
  - `sc_arc` - unique topological paths, arcs either meet two other arcs
    at a node, or include no nodes
  - `sc_node` - unique nodes
  - `unjoin` - a function to *un join* a table, the opposite of the
    database join

The `unjoin` is a bit out of place here, but it’s a key step when
building these models, used to remove duplication at various levels.
It’s the primary mechanism for *defining and building-in* topology,
which is precisely the relationships between entities in a model. This
function is published in the [CRAN package
unjoin](https://CRAN.R-project.org/package=unjoin).

# What about simple features?

Silicate is not about simple features, it’s about transcending those
limitations for day to day data problems. Unfortunately we inevitably
have to couch this work in that context.

Modern geospatial science needs normal-form data structures.

Modern GIS standards generally represent spatial data as nested lists,
whether in accordance with the Simple Features (SF) standard of the Open
Geospatial Consortium, or in `geojson` format. Most commonly used
geometric libraries are based on one or both of these two standards. We
argue that (1) the agreed representations in modern GIS geometry
effectively restrict ongoing development of GIS as a whole, and (2) the
enforced representation of geometry as nested lists as a central form is
inefficient.

## Simple Features

SF does not address what “non-simple” features are or might be, yet
clearly these include important application domains such as GPS data,
transport networks, point clouds, computer aided design, virtual and/or
augmented reality, and 3D games. Each of these significant arenas have
their own standards which are difficult to reconcile or unite without
risking fragmentation and inefficiency.

SF and nested-list representations are limited because:

  - Shapes are not represented as topological primitives and so internal
    boundaries are precluded.
  - Shapes are represented as paths so only planar polygonal shapes are
    possible.
  - Shapes may exist in XY\[Z\[M\]\] geometry, but this is not
    extensible, with no capacity to store data against component
    geometry elements.
  - Shapes have no persistent naming of features or their components.
  - There is no capacity for internal topology of shapes or within
    collections (no vertex-, edge-, or path-sharing).

These limitations mean that SF cannot fully represent every-day data
forms from tracked objects, transport, Lidar, 3D models, statistical
graphics, topological spatial maps, TopoJSON, CAD drawings, meshes or
triangulations. Translations between geospatial forms and the grammars
of data science can be disjointed, relying on localized implementations
that are lossy or inefficient, require third party workflows, or involve
unnecessary tasks.

GIS applications generally diverge from common standards in different
ways but none currently provide a normal-form model. There is no
standard way to normalize data by detecting and removing redundancy
(topology), or to densify data (a common necessity in planning domains).
There is no standard way to extend the types although complex forms are
well established in other domains.

## Arbitrarily re-composable hierarchies

The common “well-known” formats of encoding geometry (WKB/WKT for
binary/text) represent (pre-)aggregated data, yet the input levels of
aggregation are often not directly relevant to desired or desirable
levels of aggregation for analysis. A key stage in many GIS analyses is
thus an initial disaggregation to some kind of atomic form followed by
re-aggregation.

We propose a common form for spatial data that is inherently
disaggregated, that allows for maximally-efficient on-demand
re-aggregation (arbitrarily re-composable hierarchies), and that covers
the complexity of geometric and topological types widely used in data
science and modelling. We provide tools in R for more general
representations of spatial primitives and the intermediate forms
required for translation and analytical tasks. These forms are
conceptually independent of R itself and are readily implemented with
standard tabular data structures.

There is not one single normal form that should always be used. There is
one universal form that every other model may be expressed in, but also
other forms that are better suited or more efficient for certain
domains. We show that conversion between these forms is more
straightforward and extensible than from SF or related types, but is
also readily translated to and from standard types. The most important
forms we have identified are “universal” (edges and nodes), “2D
primitives” (triangles), “arcs” (shared boundaries), and “paths”
(normalized forms of SF types).

# Installation

``` r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("hypertidy/silicate")
```

# Usage

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
#>  1  0     0    22e492aa5a
#>  2  0     1    b366494f92
#>  3  0.75  1    1f96b1d5ee
#>  4  1     0.8  e2a429817f
#>  5  0.5   0.7  1885c73d8f
#>  6  0.8   0.6  20f8d97327
#>  7  0.69  0    9fae227f6e
#>  8  0.2   0.2  63bfeb3409
#>  9  0.5   0.2  ebceab2ec1
#> 10  0.5   0.4  ac06a29105
#> 11  0.3   0.6  e2db9e7964
#> 12  0.2   0.4  2bee2e53a3
#> 13  1.1   0.63 130cdec8a7
#> 14  1.23  0.3  b49d74f376

sc_edge(x)
#> # A tibble: 15 x 3
#>    .vertex0   .vertex1   edge_     
#>    <chr>      <chr>      <chr>     
#>  1 22e492aa5a b366494f92 9b0ff335ec
#>  2 b366494f92 1f96b1d5ee c4cc5cfc17
#>  3 1f96b1d5ee e2a429817f f205a8cd70
#>  4 e2a429817f 1885c73d8f 3b7a0e044e
#>  5 1885c73d8f 20f8d97327 c41acefc99
#>  6 20f8d97327 9fae227f6e f0b1c68ca2
#>  7 9fae227f6e 22e492aa5a ba0abef5bd
#>  8 63bfeb3409 ebceab2ec1 8c8bcd8d0c
#>  9 ebceab2ec1 ac06a29105 d789eae064
#> 10 ac06a29105 e2db9e7964 0bf10e3997
#> 11 e2db9e7964 2bee2e53a3 2c1f1907ab
#> 12 2bee2e53a3 63bfeb3409 a02365f29c
#> 13 20f8d97327 130cdec8a7 be8f5937d8
#> 14 130cdec8a7 b49d74f376 af2543d4a8
#> 15 b49d74f376 9fae227f6e a262bd949a

sc_node(y)
#> # A tibble: 2 x 1
#>   vertex_   
#>   <chr>     
#> 1 9b7badb2d3
#> 2 31cef134e0

sc_arc(y)
#> # A tibble: 4 x 2
#>   arc_       ncoords_
#>   <chr>         <int>
#> 1 117ab517db        4
#> 2 241ba313a5        2
#> 3 9013d3a49f        7
#> 4 f87c4c1c0b        6
```

## silicate models

There are two kinds of models, *primitive* and *sequential*.

Primitive-based models are composed of *atomic* elements that may be
worked with arbitrarily, by identity and grouping alone.

Sequential-based models are bound to ordering and contextual
assumptions. We provide the `PATH` and `ARC` models as generic,
relational forms that provide a convenient intermediate between external
forms and primitives models. Further intermediate models exist,
including monotone and convex decompositions of polygons.

There is one universal primitives-based model, an edge-only model with
two tables at its core. Higher level structures are described by
grouping tables, with as many levels as required. Any other model can be
expressed in this form.

We also differentiate *structural primitives*, which are specializations
that are more convenient or more efficient in certain cases. These
include triangulations (2D primitives), and segment structures (1D
primitives), and could provide higher dimensional forms (3D primitives,
etc. ).

Currently, we provide support for the universal model `SC`, the
sequential models `PATH` (simple features belongs here, amongst many
others) and `ARC` (arc-node topology, TopoJSON-like, OpenStreetMap), and
structural primitives `TRI`.

In practice a segment model is trivial to generate, “SEG” but we haven’t
done that. This would be analogous to the format used by
`rgl::rgl.lines` or `spatstat::psp`.

We take care to allow for *labelling* (identity) of component elements,
without taking full responsibility for maintaining them. Random IDs are
created as needed, but any operation that works with existing IDs should
be stable with them.

## Context, and some related projects

The key difference between the silicate approach and simple features is
the separation of geometry and topology. This allows for normalization
(de-duplication) of the entities that are present or that can be
identitied. Simple features has no capacity to de-duplicate or otherwise
identify vertices, edges, paths or arcs, though tools that work with
simple features do construct these schemes routinely in order to perform
operations. When these richer, topological structures are built they are
usually then discarded and the vertices are again de-normalized and
again expressed explicitly without recording any of the relationships.
In this sense, simple features can be described as an *explicitly-stored
PATH analogue*, and is no different from the model used by shapefiles,
binary blobs in databases, and many other spatial vector formats. There
are a number of notable exceptions to this including TopoJSON,
Eonfusion, PostGIS, QGIS geometry generators, Fledermaus, Mapbox, WebGL,
Threejs, D3, AFrame, Lavavu but unfortunately there’s no overall scheme
that can unify these richer structures.

The silicate family is composed of a small number of packages that apply
the principles here, either to read from path forms or primitive forms.
As work continues some of these will be incorporated into the silicate
core, when that is possible without requiring heavy external
dependencies.

  - [scgraph](https://github.com/hypertidy/scgraph)
  - [scspatstat](https://github.com/hypertidy/scspatstat)

scdb, sctrip, scrgl, scraster, scicosa,
