

https://publish.illinois.edu/geospatialsi/workshop2/


Modern GIS very generally represent spatial data as nested lists, notably in
accordance with the Simple Features (SF) standard of the Open Geospatial
Consortium, or in `geojson` format. Most commonly used geometric libraries are
based on these two standards. We argue here the two distinct points that (1) the
inherent restrictions of SF effectively translate into restrictions in the
ondoing development of GIS as a whole, and (2) representation as nested lists is
inefficient.

## Simple Features

The remit of SF prevents any ability to address what "non-simple" features are
or might be. The main application domains that fall outside SF are GPS data and
transport networks, point clouds, CAD, VR/AR and gaming, each of whic are
very significant and large arenas with their own standards. When geospatial data
meets the boundaries of these domains there is enormous fragmentation and
inefficiency with very little standardization for workers to align with. 

SF has the following limitations:

* shapes are not represented as topological primitives and so internal boundaries are precluded.
* shapes are represented as paths so only planar polygonal shapes are possible.
* the standard allows for XY[Z[M]] geometry,but this is not extensible - there is no capacity to store data against component geometry elements, and there is no persisentent naming of features or their components.
* there is no capacity for internal topology (no vertex-, edge-, or path-sharing).

These limitations mean that SF cannot represent in-full every-day objects from GPS, 3D models, statistical graphics, topological spatial maps, TopoJSON, CAD drawings, meshes or triangulations. Translations between geospatial forms and the graphics and data grammars can be disjointed and sometimes awkward, relying on localized implementations that are lossy or inefficient, require 3rd party workflows, or involve unnecessary tasks. It doesn't seem to be widely commented on but of the major GIS applications there is no one that restricts itself to the SF standard fully, and where they diverge from it they all do it in different ways. 

SF is not a "normal form" model, there is no standard way to normalize the data by detecting and removing redundancy (topology),  or densifying data that might be present but is not (common in transport), and there's no standard way to extend the types. Coordinates are not stored together in a single set and so cannot be identified by label to store further data either with or linked to them. 

## Nested Lists

The common "well-known" formats of encoding geometry (WKB for binary; WKT
for text) represent (pre-)aggregated data, yet the input levels of aggregation
are often not directly relevant to desired or desirable levels of aggregation
for analysis. A key stage in many GIS analyses is thus an initial disaggregation
to some kind of atomic form followed by re-aggregation.

We propose a common form for spatial data that is inherently disaggregated, that
allows for maximally-efficient on-demand re-aggregation, and that covers the
complexity of geometric and topological types widely used in data science and
modelling. We are working on a framework for complex, hierarchical data that can
incorporate standard spatial types and allows working with them in a general and
flexible way. We provide tools in R for more general representations of spatial
primitives and the intermediate forms required for translation and analytical
tasks.

There is not one single normal form that should always be used. There is one universal form that every other model may be expressed in, but also other forms that are better suited or more efficient for certain domains. We show that conversion between these forms is more straightforward and extensible than from SF or related types, but is also readily created from standard types and used to create them.  The forms we've identified are "universal" (edges and nodes), "2D primitives" (triangles), "arcs" (shared boundaries), and "paths" (normalized forms of SF types). 

Treating models in this set-based way naturally requires labelling of entities, labels that can be used to relate objects and their components without always computing these relationships on the fly. 




