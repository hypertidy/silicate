
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

Design
------

Currently `BRANCH()` is the main function to decompose simple features objects.

There are decomposition functions for lower-level `sf` objects organized as `sc_branch`, `sc_coord`, and `sc_object`. `sc_branch` does all the work, building a simple map of all the parts and the vertex count. This is used to classify the vertex table when it is extracted, which makes the unique-id management for branch-vertex normalization much simpler than it was in `gris` or `rangl`.

Example - sf to ggplot2 round trip
----------------------------------

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
#> 1        1       27 a52fd8cd36b9d152 9e21f32e554ad268
#> 2        1       26 8809e6d79cad11d1 14cc7b8f881750aa
#> 3        1       28 b3d966d034118451 f4cc56fd2d705bf0
#> 4        1       26 e08c3ccd2c09a22a b304ba2fbeba0145
#> 5        2        7 87d27fede3df7d33 b304ba2fbeba0145
#> 6        3        5 62f41841c0a65b68 b304ba2fbeba0145
#> 7        1       34 0679fd885ce7ff42 a6d150dc15ade4ad
#> 8        1       22 7acdfa2b443b528f d1aa560c15e62462
#> 9        1       24 56062ab4af387c89 4c774d1957b25d81
#> 10       1       17 f7c5ffa60a7b7835 62055a68b7907d11
#> # ... with 98 more rows
#> 
#> $vertex
#> # A tibble: 1,255 × 3
#>           x_       y_          vertex_
#>        <dbl>    <dbl>            <chr>
#> 1  -81.47276 36.23436 f5d826f04a76ad44
#> 2  -81.54084 36.27251 83d48773e031efc7
#> 3  -81.56198 36.27359 7e568637759075d2
#> 4  -81.63306 36.34069 bf0d516fd0bfa690
#> 5  -81.74107 36.39178 0f13bb447c10e4ff
#> 6  -81.69828 36.47178 121dc0205e247d2f
#> 7  -81.70280 36.51934 2a46afd197fb6e5f
#> 8  -81.67000 36.58965 03a892c00a974d4f
#> 9  -81.34530 36.57286 7065077270009403
#> 10 -81.34754 36.53791 5671d4594344de43
#> # ... with 1,245 more rows
#> 
#> $branch_link_vertex
#> # A tibble: 2,529 × 2
#>             branch_          vertex_
#>               <chr>            <chr>
#> 1  a52fd8cd36b9d152 f5d826f04a76ad44
#> 2  a52fd8cd36b9d152 83d48773e031efc7
#> 3  a52fd8cd36b9d152 7e568637759075d2
#> 4  a52fd8cd36b9d152 bf0d516fd0bfa690
#> 5  a52fd8cd36b9d152 0f13bb447c10e4ff
#> 6  a52fd8cd36b9d152 121dc0205e247d2f
#> 7  a52fd8cd36b9d152 2a46afd197fb6e5f
#> 8  a52fd8cd36b9d152 03a892c00a974d4f
#> 9  a52fd8cd36b9d152 7065077270009403
#> 10 a52fd8cd36b9d152 5671d4594344de43
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
#>   ..$ object_ : chr [1:6] "c84417b9c7fda032" "3c38738d820f90c2" "fd6f853060067997" "c82fc5a04ce206d6" ...
#>  $ branch            :Classes 'tbl_df', 'tbl' and 'data.frame':  189 obs. of  4 variables:
#>   ..$ island_ : chr [1:189] "1" "1" "1" "1" ...
#>   ..$ ncoords_: int [1:189] 280 27 7310 68 280 88 162 119 51 71 ...
#>   ..$ branch_ : chr [1:189] "e320decba0864b36" "86676141c803c3ff" "767296bea4415dc0" "b41d1bb426af16a7" ...
#>   ..$ object_ : chr [1:189] "c84417b9c7fda032" "3c38738d820f90c2" "fd6f853060067997" "fd6f853060067997" ...
#>  $ vertex            :Classes 'tbl_df', 'tbl' and 'data.frame':  30835 obs. of  3 variables:
#>   ..$ x_     : num [1:30835] 1116371 1117093 1117172 1117741 1117629 ...
#>   ..$ y_     : num [1:30835] -458419 -457111 -456893 -456561 -455510 ...
#>   ..$ vertex_: chr [1:30835] "c47c543938316113" "8f28f499b619d61d" "d40284204bfbcba9" "9d9b7cd1a000e454" ...
#>  $ branch_link_vertex:Classes 'tbl_df', 'tbl' and 'data.frame':  33644 obs. of  2 variables:
#>   ..$ branch_: chr [1:33644] "e320decba0864b36" "e320decba0864b36" "e320decba0864b36" "e320decba0864b36" ...
#>   ..$ vertex_: chr [1:33644] "c47c543938316113" "8f28f499b619d61d" "d40284204bfbcba9" "9d9b7cd1a000e454" ...
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
