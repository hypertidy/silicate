
<!-- README.md is generated from README.Rmd. Please edit that file -->
sc
==

The goal of sc is to provide a general common form for complex multi-dimensional data.

See the [proposal.md](proposal.md).

This package is intended to provide support to the `common form` approach described here. There's very little in the package yet, but see these projects that are informed by this approach.

-   **rbgm** - [Atlantis Box Geometry Model](https://github.com/AustralianAntarcticDivision/rbgm), a "doubly-connected edge-list" form of linked faces and boxes in a spatially-explicit 3D ecosystem model
-   **rangl** - [Primitives for Spatial data](https://github.com/r-gris/rangl), a generalization of GIS forms with simple 3D plotting
-   **spbabel** - [Translators for R Spatial](https://github.com/mdsumner/spbabel), tools to convert from and to spatial forms, provides the general decomposition framework for branches, used by `rangl`
-   **sfct** - [Constrained Triangulation for Simple Features](https://github.com/r-gris/sfct) tools to decompose `simple features` into (non-mesh-indexed) primitives.

Example
-------

``` r
library(sf)
#> Linking to GEOS 3.5.0, GDAL 2.1.1, proj.4 4.9.3
## a MULTIPOLYGON layer
nc = st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source `C:\Users\michae_sum\R\win-library\3.3\sf\shape\nc.shp' using driver `ESRI Shapefile'
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
#> 1        1       27 f5c3fa130845d200 e3b8294d9b66281f
#> 2        1       26 eecca7df4a536019 1f234ef494dcd33e
#> 3        1       28 7bded48ae8d3ab43 89b3677e352a6cf8
#> 4        1       26 f6080203ed35d389 c3c8dd58af35e1e6
#> 5        2        7 25ec270924fb11c1 c3c8dd58af35e1e6
#> 6        3        5 07be38a18a7e1216 c3c8dd58af35e1e6
#> 7        1       34 ecf8699aa2ff4cd9 9626dfa7158069ee
#> 8        1       22 0a84e9e6e1b2ec3d a12fa2bf66ffd557
#> 9        1       24 b64fed5ec6b9df6c 471ad094a56befad
#> 10       1       17 35da6d61e6272ffc 1a9734a800da8c31
#> # ... with 98 more rows
#> 
#> $vertex
#> # A tibble: 1,255 × 3
#>           x_       y_          vertex_
#>        <dbl>    <dbl>            <chr>
#> 1  -81.47276 36.23436 d4056dcf86cc3b74
#> 2  -81.54084 36.27251 24520b70ce4b2507
#> 3  -81.56198 36.27359 4a883a169945fe94
#> 4  -81.63306 36.34069 a290775b1d5db88a
#> 5  -81.74107 36.39178 dfdd022e5b261234
#> 6  -81.69828 36.47178 fc4c7533eefa546c
#> 7  -81.70280 36.51934 111263a98dc4e8e8
#> 8  -81.67000 36.58965 8a37955068a1c917
#> 9  -81.34530 36.57286 a2dd26a356e5b702
#> 10 -81.34754 36.53791 4479fba3c64a9b4e
#> # ... with 1,245 more rows
#> 
#> $branch_link_vertex
#> # A tibble: 2,529 × 2
#>             branch_          vertex_
#>               <chr>            <chr>
#> 1  f5c3fa130845d200 d4056dcf86cc3b74
#> 2  f5c3fa130845d200 24520b70ce4b2507
#> 3  f5c3fa130845d200 4a883a169945fe94
#> 4  f5c3fa130845d200 a290775b1d5db88a
#> 5  f5c3fa130845d200 dfdd022e5b261234
#> 6  f5c3fa130845d200 fc4c7533eefa546c
#> 7  f5c3fa130845d200 111263a98dc4e8e8
#> 8  f5c3fa130845d200 8a37955068a1c917
#> 9  f5c3fa130845d200 a2dd26a356e5b702
#> 10 f5c3fa130845d200 4479fba3c64a9b4e
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
