
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/hypertidy/silicate.svg?branch=master)](https://travis-ci.org/hypertidy/silicate) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/silicate?branch=master&svg=true)](https://ci.appveyor.com/project/hypertidy/silicate) [![Coverage Status](https://img.shields.io/codecov/c/github/hypertidy/silicate/master.svg)](https://codecov.io/github/hypertidy/silicate?branch=master)

silicate
========

The goal of silicate is to provide a general common form for complex multi-dimensional data.

NOTE: as of January 2018 major work is going on in the **[pslg](https://github.com/hypertidy/silicate/tree/pslg)** branch, this will be disruptive so **master** will remain stable in the original PATH/PRIMITIVE form until we figure the edge-first scheme.

There are two main motivations for `silicate`:

-   to provide a central common-form of structure data, and a "universal converter"
-   to work with topological primitives for analysis and interaction.

There are two kinds of models, *primitive* and *sequential*.

Primitive-based models are composed of *atomic* elements that may be worked with arbitrarily, by identity and grouping alone.

Sequential-based models are bound to ordering and contextual assumptions. We provide the `PATH` and `ARC` models as generic, relational forms that provide a convenient intermediate between external forms and primitives models. Further intermediate models exist, including monotone and convex decompositions of polygons.

There is one universal primitives-based model, an edge-only model with two tables at its core. Higher level structures are described by grouping tables, with as many levels as required. Any other model can be expressed in this form.

We also differentiate *structural primitives*, which are specializations that are more convenient or more efficient in certain cases. These include triangulations (2D primitives), and segment structures (1D primitives), and could provide higher dimensional forms (3D primitives, etc. ).

Currently, we provide support for the universal model `SC`, the sequential models `PATH` (simple features) and `ARC` (arc-node topology, TopoJSON-like), and structual primitives `TRI`.

In practice a segment model is trivial to generate, "SEG" but we haven't done that.

We take care to allow for *labelling* (identity) of component elements, without taking full responsibility for maintaining them. Random IDs are created as needed, but any operation that works with existing IDs should be stable with them.

We have the following worker verbs that are used to build the above models, and work between what each model offers.

-   `sc_object` - highest level properties, the "features"
-   `sc_coord` - all instances of coordinates, labelled by vertex if the source model includes them
-   `sc_vertex` - only unique coordinates (in some geometric space)
-   `sc_path` - individual paths, sequential traces
-   `sc_edge` - unique binary relations, unordered segments
-   `sc_segment` - all instances of edges
-   `sc_arc` - unique topological paths, arcs either meet two other arcs at a node, or include no nodes
-   `sc_node` - unique nodes

This scheme (in the [pslg](https://github.com/hypertidy/silicate/tree/pslg) branch) provides most of the infrastructure that we need to work with a wide variety of spatial and hierarchical structures. Pending work relates to identity management and efficiency for the universal model. The goal is for all model functions to be capable of transforming to any other model type, and for all worker verbs to work on any model (even if of limited utility).

WIP

(older readme)
--------------

Central is the `PATH` model, a normalized form of spatial data that is an *intermediate form* between standard explicit path forms and topological structures composed of simpler elements. PATH and its counterpart SC is a dual-view of the two main types of structures used in complex data. We anticipate that SC will be the universal core, and PATH more specialized along with other types such as triangulations.

*Paths* are the turtle-head-down coordinate lists used by lines, polygons, polypath, geom\_line.

*Primitives* are the edge-lists or triangle-lists or quad-lists used in rgl and in many topological structures. The key thing that makes them topological is a unique-vertex-pool, indexed by other types.

The need to provide a language of conversions between these forms for spatial data is illustrated here:

<http://rpubs.com/cyclemumner/305595>

Paths can be partly topological (as per PATH) in that a unique vertex pool is indexed by variable-length paths, and this is a key distinction from primitives which have a constant number of indexed vertices per element. There's a clash here, because most efficient for paths is very different from most efficient for primitives.

`silicate` provides an intermediate form for paths, all instances of all coordinates in one table, and another "path" table that records how many of the coordinates (in native order) are used per path. So this is a kind of *run length encoding* structure, it provides a common model that can be used by any path-based structure for a decomposition or re-composition form. A family of other functions provide the other component elements that we need, including segments, edges, arcs, nodes and vertices.

Worker functions
================

There are key worker functions `sc_coord`, `sc_object`, `sc_path` which provide the basis for interpreting simple features from a silicate perspective.

-   sc\_coord returns the table of coordinates completely flattened, and with no normalization
-   sc\_object returns the highest level feature metadata
-   sc\_path returns the table of individual paths, with a coordinate count

From these three components we can generate other forms, and we can round-trip back to the original features.

There are further workers for the ***vertices*** (`sc_node`), and ***paths*** (`sc_arc`, `sc_segment`, `sc_edge`) which provide the necessary lower-level components. These inherently work with identitifiers for components, and so they only make sense with reference to a `SC` silicate form.

-   sc\_node returns only those vertices that are met by three or more paths
-   sc\_arc returns the paths between nodes, and standalone paths that don't visit any nodes
-   sc\_segment returns all *instances* of any two-vertex line segment (directed)
-   sc\_edge returns only those unique edges, which are inherently undirected

(We possibly also want `sc_vertex` to return only the unique coordinates, but that can be obtained from `PATH` - though this area needs some care since unique might be in X-Y, Y-Z or X-Y-Z or any geometric space).

This generic set of workers is chosen because we often want the complete set of vertices in their pure form. Returning them with no grouping or identifiers and without any de-duplication means we have a representation of the pure geometry. Since the table has no other columns, generic code can be sure that all columns contain a coordinate. That means we don't need specialist code for 'XYZ', 'XYZM', 'XYT', 'TYX' and so on.

The table of individual paths records which object it belongs to, how many coordinates there are and an ID for the path. This is not intended to be 'relational', it's an intermediate form link by pure indexing. Inserting more levels between the paths and the highest objects is possible, but unclear exactly how to do this yet.

The `unjoin` concept is key for mapping the key between unique vertices and a path's instance of those as coordinates, in the right order. We can use the unjoin engine to add structure to other more generic data streams, like GPS, animal tracking, and general sensors.

**NOTE:** An early implentation had PATH and PRIMITIVE forms, but the latter was too simplistic for real use. The functions `sc_arc`, `sc_node`, `sc_segment` and `sc_edge` were added to the basic `sc_coord`, `sc_path` and `sc_object` as these are all that are required to move between any data structure in simple features or topological form. This is an ongoing area of development. See also [sphier](https://github.com/hypertidy/sphier) and [svgplotr](https://github.com/hypertidy/svgplotr) for related work.

The model functions `SC` and `PATH` should work in the following cases.

-   to flip from one to another `SC(PATH(SC(x)))` should work for any kind of 'x' model
-   for any `sf` object
-   convert to igraph objects WIP: <https://github.com/hypertidy/scgraph>
-   spatstat objects WIP: <https://github.com/hypertidy/scspatstat>
-   ...

The classes for all variants of simple features are not worked out, for instance a MULTIPOINT can end up with a degenerate (and expensive) segment table.

More functions `sc_uid` provides unique IDs, and `sc_node` is a worker for a arc-node intermediate model.

Intermediate models

-   Arc-node (WIP)
-   Monotone polygons (future work)

Design
------

There is a hierarchy of sorts with layer, object, path, primitives, coordinates, and vertices.

The current design uses capitalized function names `SC`, `PATH` ... that act on layers, while prefixed lower-case function names produce or derive the named entity at a given level for a given input. E.g. `sc_path` will decompose all the geometries in an `sf` layer to the PATH model and return them in generic form. `PATH` will decompose the layer as a whole, including the component geometries.

`PATH()` is the main model currently used to decompose inputs. Soon `SC` will be the more general, and universal core model from which other models derive or specialize.

PATH is the more general form of the GIS idioms (simple features and georeferenced raster data) This treats connected *paths* as fully-fledged entities like vertices and objects are, creating a relational model that stores all *vertices* in one table, all *paths* in another, and and all highest-level *objects* in another. The PATH model also takes the extra step of *normalizing* vertices, finding duplicates in a given geometric space and creating an intermediate link table to record all *instances of the vertices*. The PATH model does not currently normalize paths, but this is something that could be done, and is close to what arc-node topology is.

There are decomposition functions for lower-level `sf` objects organized as `sc_path`, `sc_coord`, and `sc_object`. `sc_path` does all the work, building a simple map of all the parts and the vertex count. This is used to classify the vertex table when it is extracted, which makes the unique-id management for path-vertex normalization much simpler than it was in `gris` or `rangl`.

Key examples
============

the key ones are OSM-like data with names, LiDAR data (xyz multipoints with time, colour, intensity, groupings, etc. - with groupings such as "contiguous surface" - lidR is good for this), animal track data - grouped-multilines with time, depth, temperature etc. on the coordinates, and triangulations - again grouped-structures from rgl

**NOTE:** earlier versions of this used the concept of "branch" rather than path, so there is some ongoing migration of the use of these words. *Branch* is a more general concept than implemented in geo-spatial systems generally, and so *path* is more accurate We reserve branch for possible future models that are general. A "point PATH" has meaning in the sense of being a single-vertex "path", and so a multipoint is a collection of these degenerate forms. "Path" as a concept is clearly rooted in optimization suited to planar forms, and so is more accurate than "branch".

In our terminology a branch or path is the group between the raw geometry and the objects, and so applies to a connected polygon ring, closed or open linestring, a single coordinate with a multipoint (a path with one vertex). In this scheme a polygon ring and a closed linestring are exactly the same (since they actually are exactly the same) and there are no plane-filling branches, or indeed volume-filling branches. This is a clear limitation of the branch model and it matches that used by GIS.

Exceptions
----------

There are a number of notable exceptions in the spatial world, but unfortunately this highlights how fragemented the landscape is.

TopoJSON, Eonfusion, PostGIS, QGIS geometry generators, Fledermaus, ...

Other technologies are Mapbox, WebGL, Threejs, D3, AFrame, Lavavu ...

The silicate family is composed of a small number of packages that apply the principles here, either to read from path forms or primitive forms.

-   [scgraph](https://github.com/hypertidy/scgraph)
-   [scspatstat](https://github.com/hypertidy/scspatstat)

scdb, sctrip, scrgl, scraster, scicosa,
