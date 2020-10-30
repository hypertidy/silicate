
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis-CI Build
Status](http://badges.herokuapp.com/travis/hypertidy/silicate?branch=master&env=BUILD_NAME=trusty_release&label=linux)](https://travis-ci.org/hypertidy/silicate)
[![Build
Status](http://badges.herokuapp.com/travis/hypertidy/silicate?branch=master&env=BUILD_NAME=osx_release&label=osx)](https://travis-ci.org/hypertidy/silicate)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/silicate?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/silicate)
[![Coverage
Status](https://img.shields.io/codecov/c/github/hypertidy/silicate/master.svg)](https://codecov.io/github/hypertidy/silicate?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/silicate)](https://cran.r-project.org/package=silicate)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/silicate)](https://cran.r-project.org/package=silicate)
[![R build
status](https://github.com/hypertidy/silicate/workflows/R-CMD-check/badge.svg)](https://github.com/hypertidy/silicate/actions)

# Overview

The goal of silicate is to bridge planar geospatial data types with
flexible mesh data structures and visualization.

We aim to provide

  - a *common-form* for representing hierarchical data structures
  - a *universal converter* between complex data types
  - topological primitives for analysis and exploration.

The core of silicate are *worker functions* that are generic and work
with any kind of data that has hierarchical structure. These functions
work on *models*, that include various formats (sf, sp, trip) and also
on silicate models themselves.

## Functions

We have the following worker verbs, designed to work with many spatial
data formats and with silicate’s own structures.

  - `sc_object()` - highest level properties, the “features”
  - `sc_coord()` - all instances of coordinates, labelled by vertex if
    the source model includes them
  - `sc_vertex()` - only unique coordinates (in some geometric space)
  - `sc_path()` - individual paths, sequential traces
  - `sc_edge()` - unique binary relations, unordered segments (segments
    and edges are currently under review, and may change)
  - `sc_segment()` - all instances of edges
  - `sc_arc()` - unique topological paths, arcs either meet two other
    arcs at a node, or include no nodes
  - `sc_node()` - unique nodes

The idea is that each function can return the underlying entities of a
data object, no matter its underlying format. This interoperability
design contrasts with major spatial packages that require format
peculiarities in order to work, even when these details are not
relevant.

## Models

Silicate defines a number of key models to represent various
interpretations of hierarchical (usually spatial) data. These are `SC`,
`PATH`, `ARC` and `TRI`. Most models have a counterpart
*structurally-optimized* version with a similar name: `SC0`, `PATH0`,
`TRI0`. Other models are possible, and include `DEL` in the
[in-development anglr package](https://github.com/hypertidy/anglr/) that
extends `TRI`.

Each model is composed of a type of *primitive*, and each provides a
normalization (de-duplication) of geometry for efficiency and or
topology. Silicate quite deliberately separates the concepts of geometry
and topology completely. Primitives define the topology (edges, paths,
arcs, or triangles) and the vertex table defines the geometry. We
reserve the names `x_`, `y_`, `t_` (time) and `z_` (elevation) for the
usual geometric dimensions, and these are treated specially by default.
No limit is put geometric dimension however, it’s possible to store
anything at all on the vertex table. Some models include an `object`
table, and this represents a higher level grouping of primitives (and
corresponds to *features* in SF).

The most general model is `SC`, composed of three tables `vertex`,
`edge` and `object` and all entities are explicitly labelled. Indexes
between tables are unique and persistent and arbitrary, they can be
arbitrarily accessed. This is closely related to the more bare-bones
`SC0` model, composed of only two tables `vertices`, and `objects`.
These are related *structurally* by nesting the relations within the
object table. Here the relations are not persistent, so we can subset
the objects but we cannot change the vertex table with updating these
indexes.

`SC0` can deal with 0-dimensional topology types (points) as well as
1-dimensional types (edges), but `SC` is strictly for edges.

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
  - `SC0` is a stripped down structural model analogous to `SC`, there
    are only implicit relations of object to vertices, with a nested
    list of edge indexes

The models `PATH0` and `ARC0` are in-development. By analogy to `SC0`
they will be composed of two tables, `object` and `vertex` with nested
structural-index tables on `object` holding the path and arc indexes
that are row numbers of `vertex`. It’s not clear yet if this vertex
table should be de-duplicated.

Earlier versions included a mix of these models, and the definitions
have changed many times. Still a work-in-progress.

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

There is another important function `unjoin()` use to normalize tables
that have redundant information. The `unjoin()` isthe opposite of the
database join, and has a nearly identical counterpart in [the dm
package](https://github.com/krlmlr/dm) with its `decompose_table()`.
Unjoin is the same as `tidyr::nest()` but returns two tables rather than
splitting one into the rows of other.

The `unjoin` is a bit out of place here, but it’s a key step when
building these models, used to remove duplication at various levels.
It’s the primary mechanism for *defining and building-in* topology,
which is precisely the relationships between entities in a model. This
function is published in the [CRAN package
unjoin](https://CRAN.R-project.org/package=unjoin).

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
#> 
#> Attaching package: 'silicate'
#> The following object is masked from 'package:stats':
#> 
#>     filter
x <- SC(minimal_mesh) ## convert simple features to universal form

y <- ARC(minimal_mesh) ## convert simple features to "arc-node" form
```

Obtain the elements of a known model type.

``` r
sc_vertex(x)
#> # A tibble: 14 x 3
#>       x_    y_ vertex_
#>    <dbl> <dbl> <chr>  
#>  1  0     0    BTjJfg 
#>  2  0     1    Os5aPK 
#>  3  0.2   0.2  J8G4eF 
#>  4  0.2   0.4  MCs4EU 
#>  5  0.3   0.6  nd6fm2 
#>  6  0.5   0.2  cexo43 
#>  7  0.5   0.4  zifu2p 
#>  8  0.5   0.7  acrNba 
#>  9  0.69  0    Kq3sSr 
#> 10  0.75  1    LpEEWk 
#> 11  0.8   0.6  OdWVZU 
#> 12  1     0.8  r8NZuW 
#> 13  1.1   0.63 pdkEIF 
#> 14  1.23  0.3  lXqOcw

sc_edge(x)
#> # A tibble: 15 x 4
#>    .vx0   .vx1   path_ edge_ 
#>    <chr>  <chr>  <int> <chr> 
#>  1 BTjJfg Os5aPK     1 mAEnCR
#>  2 Os5aPK LpEEWk     1 9V21Nk
#>  3 LpEEWk r8NZuW     1 oo2Zad
#>  4 acrNba r8NZuW     1 XxpY1a
#>  5 acrNba OdWVZU     1 7Rce0p
#>  6 Kq3sSr OdWVZU     1 g3F3EJ
#>  7 BTjJfg Kq3sSr     1 JymcBh
#>  8 J8G4eF cexo43     2 PkVR4T
#>  9 cexo43 zifu2p     2 XGLFtj
#> 10 nd6fm2 zifu2p     2 C9pruP
#> 11 MCs4EU nd6fm2     2 wdao9g
#> 12 J8G4eF MCs4EU     2 C7Uzol
#> 13 OdWVZU pdkEIF     3 IjDwzV
#> 14 pdkEIF lXqOcw     3 xYoBec
#> 15 Kq3sSr lXqOcw     3 KNTYxL

sc_node(y)
#> # A tibble: 2 x 1
#>   vertex_
#>   <chr>  
#> 1 JibOO4 
#> 2 jGfrE0

sc_arc(y)
#> # A tibble: 4 x 2
#>   arc_   ncoords_
#>   <chr>     <int>
#> 1 2hVvpA        2
#> 2 F3YaTb        7
#> 3 HqTb6m        5
#> 4 yu4nxb        4
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

The [spacebucket](https://github.com/mdsumner/polymer) (arbitrary
multi-layer polygonal overlays) and
[sphier](https://github.com/hypertidy/sphier/) (generic hierarchies from
atomic forms) show two different approaches to the problem of
hierarchical data and flexible representations.

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

Looking for a music reference? I always am: Child’s Play, by Carcass.

Please note that the ‘silicate’ project is released with a [Contributor
Code of
Conduct](https://github.com/hypertidy/silicate/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
