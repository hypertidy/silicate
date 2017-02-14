---
title: "General forms for hierarchical data"
author: "Michael Sumner"
date: "January 2017"
output: html_document
---

Applicant: [Michael Sumner](https://github.com/mdsumner/), [Australian Antarctic Division](http://www.antarctica.gov.au/); [mdsumner@gmail.com](mailto:mdsumner@gmail.com)

Supporting Authors: Simon Wotherspoon, Jessica Melbourne-Thomas, Phillipa Bricher


# The problem

There is no common  grammar of spatial data that covers the complexity of geometric and topological types widely used in R. The translation between geo-spatial forms and the graphics and data grammars is disjointed and sometimes awkward, relying on localized implementations that can be lossy or inefficient, require 3rd party workflows, and sometimes involve unnecessary tasks. 

Simple features is seen as a corner-stone resource for a central basis for translations but it is only able to provide this for a subset of the wider remit of "spatial data" in R. Topology in the form of component-element sharing (indexing of vertex, edge, arc, path) is not available to simple features, and while there are tools to generate it for certain  planar cases, these  are not explicitly available outside provided workflows. 

Visualization and interactive exploration tools are used to augment raw spatial data in terms of groups, mappings and scales but there is only limited ways to represent these augmented forms and work with them. The richness in R's specialist forms currently lacks a central language for conversion to generic storage and transmission. Most formats are either purely geometry and topology and fields with no aesthetics, or pure aesthetics baked-in to graphical primitives without the original data used to create the mappings. 

## Motivations

The simple features standard has the following limitations meaning that it cannot represent in-full every day objects from GPS, `rgl`, `ggplot2`/`ggvis`, `spatstat`, `maps`, TopoJSON, CAD drawings, 3D and general model structures. 

* shapes are represented as paths so only planar polygonal shapes are possible
* the standard allows for `XY[Z[M]]` geometry but is not extensible  - no capacity to store data against component geometry elements 
* no capacity for internal topology (no vertex-, edge-, or path-sharing). 

That simple features cannot store these in full means that many translation patterns either result in loss of information from the original form or require overloaded workarounds to keep track of the information outside of the core translation and re-apply it. Translations that are common are from format to format,  coordinate system geometry transformations, shape-modifying transformations. 


# The plan

Request for advice on key parties to contribute, funding for working groups and presentations. 

Investigate best options for front-end user interfaces and back-end systems. 

* lists of tables, as illustrated in proto-forms in spbabel, rangl, rbgm
* sf-like forms, list-columns with shared-entity semantics
* advanced techniques, environments, R6, with vertex/primitives pools
* database or database-like connections in list-columns - nested tibbles that are back-ended?

Key outputs

1. Provide tools for decomposing geo-spatial and other complex data to common general forms, including topological indexing. 
2. Illustrate general workflow with tools to convert between `sf`, GeoJSON, TopoJSON, leaflet list-forms, and `rgl` and `plotly`
3. Generate a classification of the broad class of "spatial data" in R that incorporates simple features and other forms and guides translation efforts across R packages. These are patterns that are for the most part user-accessible, so creating modified or specialized versions that are more efficient or better focussed for particular tasks will be straightforward. 
4. Implement a prototype general-form geo-spatial-graphics data structure that can store geometry, topology, aesthetic mappings to bridge the creation of hierarchical data in the tidyverse with its visualization and analysis. 

