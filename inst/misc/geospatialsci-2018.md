# Arbitrarily re-composable hierarchies /OR/ Inherent disaggregation: modern geospatial needs normal-form data structures

Michael D. Sumner, Mark Padgham and Angela Li

Modern GIS standards represent spatial data as nested lists, in accordance with
the Simple Features (SF) standard of the Open Geospatial Consortium, or in
`geojson` format. Most commonly used geometric libraries are based on these two
standards. We argue that (1) the agreed restrictions in modern GIS geometry
effectively restrict ongoing development of GIS as a whole, and (2) the enforced
representation of geometry as nested lists as a central form is inefficient.

## Simple Features

The remit of SF does not address what "non-simple" features are or might be but
clearly these include important application domains such as GPS data, transport
networks, point clouds, Computer Aided Design, virtual and/or augmented reality
and 3D games. Each of these are very significant and large arenas with their own
standards that don't fall under any single domain. Geospatial data is extremely
varied and widespread and and so inevitably meets the boundaries of these
domains with usually a lot of fragmentation and inefficiency.

SF inherently has these main limitations:

* shapes are not represented as topological primitives and so internal boundaries are precluded.
* shapes are represented as paths so only planar polygonal shapes are possible.
* shapes may exist in XY[Z[M]] geometry,but is not extensible, with no capacity to store data against component geometry elements. 
* shapes have  no persistent naming of features or their components.
* no capacity for internal topology of shapes or within collections (no vertex-, edge-, or path-sharing).

These limitations mean that SF cannot represent in-full every-day objects from tracked objects, transport, Lidar, 3D models, statistical graphics, topological spatial maps, TopoJSON, CAD drawings, meshes or triangulations. Translations between geospatial forms and the grammars of data science can be disjointed, relying on localized implementations that are lossy or inefficient, requiring third party workflows, or involving unnecessary tasks. 

The major GIS applications all diverge from the common standard in different ways but none currently provide a normal-form model. There is no standard way to normalize data by detecting and removing redundancy (topology), or densifying data that might be present but is not (common in transport). There's no standard way to extend the types although complex forms are well extablished in other domains.  

## Nested Lists

The common "well-known" formats of encoding geometry (WKB for binary; WKT
for text) represent (pre-)aggregated data, yet the input levels of aggregation
are often not directly relevant to desired or desirable levels of aggregation
for analysis. A key stage in many GIS analyses is thus an initial disaggregation
to some kind of atomic form followed by re-aggregation.

We propose a common form for spatial data that is inherently disaggregated, that
allows for maximally-efficient on-demand re-aggregation (arbitrarily re-composable hierarchies), and that covers the
complexity of geometric and topological types widely used in data science and
modelling. We provide tools in R for more general representations of spatial
primitives and the intermediate forms required for translation and analytical
tasks that are readily implemented with standard tabular data structures. 

There is not one single normal form that should always be used. There is one universal form that every other model may be expressed in, but also other forms that are better suited or more efficient for certain domains. We show that conversion between these forms is more straightforward and extensible than from SF or related types, but is also readily created from standard types and used to create them.  The forms we've identified are "universal" (edges and nodes), "2D primitives" (triangles), "arcs" (shared boundaries), and "paths" (normalized forms of SF types). 




