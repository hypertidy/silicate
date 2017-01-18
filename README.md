
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.com/mdsumner/sc.svg?branch=master)](https://travis-ci.com/mdsumner/sc)

sc
==

The goal of sc is to provide a general common form for complex multi-dimensional data.

See the [proposal.md](proposal.md).

Why?
====

Geographic Information System (GIS) tools provide data structures optimized for a relatively narrow class of workflows that leverage a combination of *spatial*, graphics, drawing-design, imagery, geodetic and database techniques. When modern GIS was born in the 1990s it adopted a set of compromises that divorced it from its roots in graph theory (arc-node topology) to provide the best performance for what were the most complicated sets of cartographic and land-management system data at the time.

The huge success of ArcView and the shapefile brought this arcane domain into common usage and helped establish our modern view of what "geo-spatial data" is. The creation of the "simple features standard"" in the early 2000s formalized this modern view and provided a basis to avoid some of the inconsistencies and incompleteness that are present in the shapefile specification.

Spatial, graphics, drawing-design, imagery, geodetic and database techniques are broader than any GIS, are used in combination in many fields, but no other field combines them in the way that GIS tools do. GIS does however impose a certain view point, a lens through which each of those very general fields is seen via the perspective of the optimizations, the careful constraints and compromises that were formalized in the early days.

This lens is seen side-on when 1) bringing graphics data (images, drawings) into a GIS where a localization metadata context is assumed 2) attempting to visualize geo-spatial raster data with graphics tools 3) creating lines to represent the path of sensor platforms that record many variables like temperature, salinity, radiative flux as well as location in time and space.

The word "spatial" has a rather general meaning, and while GIS idioms sometimes extend into the Z dimension time is usually treated in a special way. Where GIS really starts to show its limits is in the boundary between discrete and continuous measures and entities. We prefer to default to the most general meaning of spatial, work with tools that allow flexibility despite the (rather arbitrary) choice of topological and geometric structures and dimensions that a given model needs. When the particular optimizations and clever constraints of the simple features and GIS world are required and/or valuable then we use those, but prefer not to see that 1) this model must fit into this GIS view 2) GIS has no place in this model. For us the boundaries are not so sharp and there's valuable cross-over in many fields.

The particular GIS-like limitations that we seek are as follows.

-   flexibility in the number and type/s of attribute stored as "coordinates", x, y, lon, lat, z, time, temperature, etc.
-   ability to store attributes on parts i.e. the state is the object, the county is the part
-   shared vertices
-   the ability to leverage topology engines like D3 to dynamically segmentize a piecewise graph using geodetic curvature
-   the ability to extend the hierarchical view in GIS to 3D, 4D spatial, graphical, network and general modelling domains
-   clarity on the distinction between topology and geometry
-   clarity on the distinction between vector and raster data, without having an arbitrary boundary between them
-   multiple models of raster `georeferencing` from basic offset/scale, general affine transform, full curvilinear and partial curvilinear with affine and rectilinear optimizations where applicable
-   ability to store points, lines and areas together, with shared topology as appropriate
-   a flexible and powerful basis for conversion between formats both in the GIS idioms and outside them
-   flexibility, ease-of-use, composability, modularity, tidy-ness
-   integration with specialist computational engines, database systems, geometric algorithms, drawing tools and other systems
-   interactivity, integration with D3, shiny, ggplot2, ggvis, leaflet
-   scaleability, the ability to leverage back-end databases, specialist parallelism engines, remote compute and otherwise distributed compute systems

Flexibility in attributes generally is the key to breaking out of traditional GIS constraints that don't allow clear continuous / discrete distinctions, or time-varying objects/events, 3D/4D geometry, or clarity on topology versus geometry. When everything is tables this becomes natural, and we can build structures like link-relations between tables that transfer data only when required.

The ability many GIS tools from R in a consistent way is long-term goal, and this will be best done via dplyr "back-ending" or a model very like it.

Approach
========

We can't possibly provide all the aspirations detailed above, but we hope to

-   demonstrate the clear need, interest and opportunities that currently exist for fostering their development
-   illustrate links between existing systems that from a slightly different perspective become achievable goals rather than insurmountable challenges
-   provide a platform for generalizing some of the currently fragmented translations that occur across the R community between commonly used tools that aren't formally speaking to each other.
-   provide tools that we build along the way

This package is intended to provide support to the `common form` approach described here. The package is not fully functional yet, but see these projects that are informed by this approach.

-   **rbgm** - [Atlantis Box Geometry Model](https://github.com/AustralianAntarcticDivision/rbgm), a "doubly-connected edge-list" form of linked faces and boxes in a spatially-explicit 3D ecosystem model
-   **rangl** - [Primitives for Spatial data](https://github.com/r-gris/rangl), a generalization of GIS forms with simple 3D plotting
-   **spbabel** - [Translators for R Spatial](https://github.com/mdsumner/spbabel), tools to convert from and to spatial forms, provides the general decomposition framework for branches, used by `rangl`
-   **sfct** - [Constrained Triangulation for Simple Features](https://github.com/r-gris/sfct) tools to decompose `simple features` into (non-mesh-indexed) primitives.

Design
------

We use the following words in a specified sense, denoting a hierarchy of sorts in order from highest to lowest with layer, object, branch (or path), primitives, coordinates, and vertices.

The current design uses capitalized function names `BRANCH`, `PRIMITIVE` ... that act on layers, while prefixed lower-case function names produce or derive the named entity at a given level for a given input. E.g. `sc_branch` will decompose all the geometries in an `sf` layer to the BRANCH model and return them in generic form. `BRANCH` will decompose the layer as a whole, including the component geometries.

`BRANCH()` is the main model used to decompose inputs, as it is the a more general form of the GIS idioms (simple features and georeferenced raster data) This treats connected *paths* as fully-fledged entities like vertices and objects are, creating a relational model that stores all *vertices* in one table, all *branches* in another, and and all highest-level *objects* in another. The BRANCH model also takes the extra step of *normalizing* vertices, finding duplicates in a given geometric space and creating an intermediate link table to record all *instances of the vertices*. The BRANCH model does not currently normalize branches, but this is something that could be done, and is close to what arc-node topology is.

The `PRIMITIVE` function decomposes a layer into actual primitives, rather than "paths", these are point, line segment, triangle, tetrahedron, and so on.

There are decomposition functions for lower-level `sf` objects organized as `sc_branch`, `sc_coord`, and `sc_object`. `sc_branch` does all the work, building a simple map of all the parts and the vertex count. This is used to classify the vertex table when it is extracted, which makes the unique-id management for branch-vertex normalization much simpler than it was in `gris` or `rangl`.

In our terminology a branch or path is the group between the raw geometry and the objects, and so applies to a connected polygon ring, closed or open linestring, a single coordinate with a multipoint (a path with one vertex). In this scheme a polygon ring and a closed linestring are exactly the same (since they actually are exactly the same) and there are no plane-filling branches, or indeed volume-filling branches. This is a clear limitation of the branch model and it matches that used by GIS.

Exceptions
----------

TopoJSON, Eonfusion, PostGIS, QGIS geometry generators, Fledermaus, ...

Example - sf to ggplot2 round trip
----------------------------------

``` r
library(sf)
#> Linking to GEOS 3.5.0, GDAL 2.1.1, proj.4 4.9.3
## a MULTIPOLYGON layer
nc = st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source `C:\Users\mdsumner\Documents\R\win-library\3.3\sf\shape\nc.shp' using driver `ESRI Shapefile'
#> Warning in CPL_read_ogr(dsn, layer, as.character(options), quiet,
#> iGeomField - : GDAL Error 1: Failed to find required field in
#> gdal_datum.csv in InitDatumMappingTable(), using default table setup.
#> converted into: MULTIPOLYGON
#> Simple feature collection with 100 features and 14 fields
#> geometry type:  MULTIPOLYGON
#> dimension:      XY
#> bbox:           xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> epsg (SRID):    4267
#> proj4string:    +proj=longlat +datum=NAD27 +no_defs
```

The common form is the entity tables, objects, branches, vertices and a link table to allow de-duplication of shared vertices. Currently this de-duplication is done on all coordinate fields, but for most applications it will usually be done only in X-Y.

``` r
library(sc)
nc = st_read(system.file("gpkg/nc.gpkg", package="sf"))
#> Reading layer `nc.gpkg' from data source `C:\Users\mdsumner\Documents\R\win-library\3.3\sf\gpkg\nc.gpkg' using driver `GPKG'
#> Simple feature collection with 100 features and 14 fields
#> geometry type:  MULTIPOLYGON
#> dimension:      XY
#> bbox:           xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> epsg (SRID):    4267
#> proj4string:    +proj=longlat +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat +no_defs

(bmodel <- BRANCH(nc))
#> $object
#> # A tibble: 100 × 15
#>     AREA PERIMETER CNTY_ CNTY_ID        NAME   FIPS FIPSNO CRESS_ID BIR74
#>    <dbl>     <dbl> <dbl>   <dbl>      <fctr> <fctr>  <dbl>    <int> <dbl>
#> 1  0.114     1.442  1825    1825        Ashe  37009  37009        5  1091
#> 2  0.061     1.231  1827    1827   Alleghany  37005  37005        3   487
#> 3  0.143     1.630  1828    1828       Surry  37171  37171       86  3188
#> 4  0.070     2.968  1831    1831   Currituck  37053  37053       27   508
#> 5  0.153     2.206  1832    1832 Northampton  37131  37131       66  1421
#> 6  0.097     1.670  1833    1833    Hertford  37091  37091       46  1452
#> 7  0.062     1.547  1834    1834      Camden  37029  37029       15   286
#> 8  0.091     1.284  1835    1835       Gates  37073  37073       37   420
#> 9  0.118     1.421  1836    1836      Warren  37185  37185       93   968
#> 10 0.124     1.428  1837    1837      Stokes  37169  37169       85  1612
#> # ... with 90 more rows, and 6 more variables: SID74 <dbl>, NWBIR74 <dbl>,
#> #   BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>, object_ <chr>
#> 
#> $branch
#> # A tibble: 108 × 4
#>    island_ ncoords_          branch_          object_
#>      <chr>    <int>            <chr>            <chr>
#> 1        1       27 016b97a9d70103b3 36b928ba583907b0
#> 2        1       26 4db6fa6cb70902c6 6d8d4685cdae7c32
#> 3        1       28 e0ff789a8f99765b e2d7bbbc580e3dfc
#> 4        1       26 d8e5939596881aee fa6d5382a8a70a8f
#> 5        2        7 24d32c5f0439ed92 fa6d5382a8a70a8f
#> 6        3        5 9dda26c3b7ed86fd fa6d5382a8a70a8f
#> 7        1       34 928950fb578295d2 bc0212c187e19393
#> 8        1       22 16608a1ee32be042 97925c0f61440d33
#> 9        1       24 80e5c7876fb8f060 f678eef5cbec7646
#> 10       1       17 8c813514a6da5cb1 64b7af1deb4f8c53
#> # ... with 98 more rows
#> 
#> $vertex
#> # A tibble: 1,255 × 3
#>           x_       y_          vertex_
#>        <dbl>    <dbl>            <chr>
#> 1  -81.47276 36.23436 b49fe0e01ddc9a09
#> 2  -81.54084 36.27251 205fb5c27bde25fb
#> 3  -81.56198 36.27359 a13905a3cfe09ef1
#> 4  -81.63306 36.34069 795869e310c82c57
#> 5  -81.74107 36.39178 92664ce4502e2374
#> 6  -81.69828 36.47178 f2753ba13ff7c694
#> 7  -81.70280 36.51934 af26d0a4360ffa1f
#> 8  -81.67000 36.58965 aabd5b3d02fe5d36
#> 9  -81.34530 36.57286 836f6012385febf4
#> 10 -81.34754 36.53791 da3431097cfcdcdb
#> # ... with 1,245 more rows
#> 
#> $branch_link_vertex
#> # A tibble: 2,529 × 3
#>             branch_ order_          vertex_
#>               <chr>  <int>            <chr>
#> 1  016b97a9d70103b3      1 b49fe0e01ddc9a09
#> 2  016b97a9d70103b3      2 205fb5c27bde25fb
#> 3  016b97a9d70103b3      3 a13905a3cfe09ef1
#> 4  016b97a9d70103b3      4 795869e310c82c57
#> 5  016b97a9d70103b3      5 92664ce4502e2374
#> 6  016b97a9d70103b3      6 f2753ba13ff7c694
#> 7  016b97a9d70103b3      7 af26d0a4360ffa1f
#> 8  016b97a9d70103b3      8 aabd5b3d02fe5d36
#> 9  016b97a9d70103b3      9 836f6012385febf4
#> 10 016b97a9d70103b3     10 da3431097cfcdcdb
#> # ... with 2,519 more rows
#> 
#> attr(,"class")
#> [1] "BRANCH" "sc"    
#> attr(,"join_ramp")
#> [1] "object"             "branch"             "branch_link_vertex"
#> [4] "vertex"
```

Prove that things work by round-tripping to the BRANCH model and onto the old fortify approach for `ggplot2`.

``` r
inner_cascade <- function(x) {
  tabnames <- sc:::join_ramp(x)
  tab <- x[[tabnames[1]]]
  for (ni in tabnames[-1L]) tab <- dplyr::inner_join(tab, x[[ni]])
  tab
}

## this just joins everything back together in one big fortify table
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
tab <- bmodel  %>% inner_cascade()
#> Joining, by = "object_"
#> Joining, by = "branch_"
#> Joining, by = "vertex_"

library(ggplot2)
ggplot(tab) + aes(x = x_, y = y_, group = branch_) + 
  geom_polygon(aes(fill = AREA)) +  geom_path(lwd = 2, col = "black") 
```

![](README-unnamed-chunk-4-1.png)

What about polygons with holes and lots of tiny complicated parts.

``` r
data("inlandwaters")

iw <- BRANCH(inlandwaters)

str(iw)
#> List of 4
#>  $ object            :Classes 'tbl_df', 'tbl' and 'data.frame':  6 obs. of  3 variables:
#>   ..$ ID      : int [1:6] 103841 103842 103843 103846 103847 103848
#>   ..$ Province: chr [1:6] "Australian Capital Territory" "New Caledonia" "New South Wales" "South Australia" ...
#>   ..$ object_ : chr [1:6] "b2a3bfaca5c1c3c0" "aa81abd954be4abf" "d15ab9c76f3f96b2" "507250c4091f87b3" ...
#>  $ branch            :Classes 'tbl_df', 'tbl' and 'data.frame':  189 obs. of  4 variables:
#>   ..$ island_ : chr [1:189] "1" "1" "1" "1" ...
#>   ..$ ncoords_: int [1:189] 280 27 7310 68 280 88 162 119 51 71 ...
#>   ..$ branch_ : chr [1:189] "0fd4befbf6c20d88" "c54cbc9c48e2c8cf" "95c29b3243e74cbd" "014fda2c434d5a4a" ...
#>   ..$ object_ : chr [1:189] "b2a3bfaca5c1c3c0" "aa81abd954be4abf" "d15ab9c76f3f96b2" "d15ab9c76f3f96b2" ...
#>  $ vertex            :Classes 'tbl_df', 'tbl' and 'data.frame':  30835 obs. of  3 variables:
#>   ..$ x_     : num [1:30835] 1116371 1117093 1117172 1117741 1117629 ...
#>   ..$ y_     : num [1:30835] -458419 -457111 -456893 -456561 -455510 ...
#>   ..$ vertex_: chr [1:30835] "7235a307d1407b97" "0d229c2010fdd762" "6ae1a777320c516d" "af2b7c19f220c2fb" ...
#>  $ branch_link_vertex:Classes 'tbl_df', 'tbl' and 'data.frame':  33644 obs. of  3 variables:
#>   ..$ branch_: chr [1:33644] "0fd4befbf6c20d88" "0fd4befbf6c20d88" "0fd4befbf6c20d88" "0fd4befbf6c20d88" ...
#>   ..$ order_ : int [1:33644] 1 2 3 4 5 6 7 8 9 10 ...
#>   ..$ vertex_: chr [1:33644] "7235a307d1407b97" "0d229c2010fdd762" "6ae1a777320c516d" "af2b7c19f220c2fb" ...
#>  - attr(*, "class")= chr [1:2] "BRANCH" "sc"
#>  - attr(*, "join_ramp")= chr [1:4] "object" "branch" "branch_link_vertex" "vertex"

tab <- iw  %>% inner_cascade()
#> Joining, by = "object_"
#> Joining, by = "branch_"
#> Joining, by = "vertex_"

library(ggplot2)
ggplot(tab) + aes(x = x_, y = y_, group = branch_) + 
  ggpolypath::geom_polypath(aes(fill = Province)) +  geom_path(col = "black") 
```

![](README-unnamed-chunk-5-1.png)

``` r

ggplot(tab %>% filter(Province == "South Australia")) + aes(x = x_, y = y_, group = branch_) + 
  ggpolypath::geom_polypath(fill = "dodgerblue") +  geom_path(col = "black") + coord_fixed()
```

![](README-unnamed-chunk-5-2.png)

Example - sf to SQLite and filtered-read
----------------------------------------

See scdb

Primitives, the planar straight line graph and TopoJSON
-------------------------------------------------------

(WIP see primitives-classes)

``` r
example(PRIMITIVE)
```

### Arc-node topoplogy

``` r

example(arc_node)
```
