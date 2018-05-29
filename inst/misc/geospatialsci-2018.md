

https://publish.illinois.edu/geospatialsi/workshop2/


> The second workshop is intended to build on the success of the first workshop with three foci: (a) gaining an in-depth understanding of which scientific problems could greatly benefit from advances in geospatial software that operates on large-scale, data-intensive, and/or computationally-intensive infrastructures; (b) identifying a suite of core technical capabilities that are necessary for geospatial big data transformation and associated scientific problem solving; and (c) beginning to plan how an institute could lead the communities to develop and benefit from those capabilities

> Specifically, if you have an interesting use case or scientific problem that requires computation- and/or data-intensive geospatial software that you believe GSI should focus on, you should make a case for it in your position paper. What are the most important scientific problems, which could be revolutionized by advances in geospatial software that could only be made possible by an NSF software institute? In other words, which geospatial software advances are needed but will not conceivably be provided by a private entity or individual researchers to solve these problems? How could better geospatial software make a difference? Please be as specific as possible.

Modern GIS very generally represent spatial data as nested lists, notably in
accordance with the Simple Features (SF) standard of the Open Geospatial
Consortium, or in `geojson` format. Most commonly used geometric libraries are
based on these two standards. We argue here the two distinct points that (1) the
inherent restrictions of SF effectively translate into restrictions in the
ongoing development of GIS as a whole, and (2) representation as nested lists is
inefficient.

## Simple Features

The remit of SF prevents any ability to address what "non-simple" features are
or might be. The main application domains that fall outside SF are GPS data and
transport networks, point clouds, CAD, VR/AR and gaming, and each of these are
very significant and large arenas with their own standards. When geospatial data
meets at the boundaries of these domains there is enormous fragmentation and
inefficiency with very little standardization for workers to align with. 

SF has the following limitations:

* shapes are represented as paths so only planar polygonal shapes are possible.
* the standard allows for XY[Z[M]] geometry,but this is not extensible - there is no capacity to store data against component geometry elements, and there is no persistent naming of features or their components.
* there is no capacity for internal topology (no vertex-, edge-, or path-sharing).

These limitations mean that SF cannot represent in-full every-day objects from GPS, 3D models, statistical graphics, topological spatial maps, TopoJSON, CAD drawings, triangulations. Translations between geo-spatial forms and the graphics and data grammars can be disjointed and sometimes awkward, relying on localized implementations that are lossy or inefficient, require 3rd party workflows, or involve unnecessary tasks. It doesn't seem to be widely commented on but of the major GIS applications there is no one that restricts itself to the SF standard fully, and where they diverge from it they all do it in different ways. 

SF is not a "normal form" model, there is no standard way to normalize the data by detecting and removing redundancy (topology),  or densifying data that might be present but is not (common in transport), and there's no standard way to extend the types. Because the coordinates are not stored as such in a single set there's no way to identify them individually by label so that extra data can be stored against them or about them. 

## Nested Lists

MS:<sup>[1](#fn1)</sup>
> GIS and geospatial are well supported in database systems however, but it is achieved using atomic, standalone serializable types. Common is a binary blob (WKB) or a text string (WKT) the "well-known" formats of encoding geometry. Geospatial systems explicitly pack and unpack these forms for SF-level geometric processing, but otherwise these atomic forms exist so that transmission is able to be done by standalone table records.

(MP:) The common "well-known" formats of encoding geometry (WKB for binary; WKT
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

Treating models in this set-based way naturally requires labelling of entities, labels that can be used to relate objects and their components without always computing these relationships on the fly. We 





## More (from silicate)

Simple features is rightly seen as a corner-stone resource, but as a central basis for translations it is only able to handle a subset of the wider remit of “spatial data” in R. Topology in the form of component-element sharing (indexing of vertex, edge, arc, path) is not available to simple features, and while there are tools to generate it for certain planar cases, these are not explicitly available outside provided workflows and this information is not generally available for extensible uses.

Translation patterns that use simple features to translate data from native formats result in loss of information requiring complicated workarounds to preserve it. Common translations include in-memory structural representations, serialized forms within file formats, coordinate system geometry transformations, and topological or shape-modifying transformations.

<a name="fn1">1</a> MP: I'm not really sure what this means?
