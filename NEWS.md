# silicate 0.4.0

* Fixed obscure bug that messed up anglr because of class-dropping for the 
sfc in a tibble. now 0.3.0.9040

* A potentially breaking change, stray uses of the `triangle$visible_` 
property have been removed, to be consistent with the anglr package. 

* Several fixes related to new unnest() behaviour, it was 
 unnecessary so replaced with use of do.call/rbind. 
 
* Fix previous unexported `sc_vertex.SC0`, the cause of https://github.com/hypertidy/anglr/issues/97. 

* Fixed a problem with unclassed sfc lists. #109 and https://github.com/hypertidy/anglr/issues/91/. 

# silicate 0.3.0

* Import crsmeta so we can navigate the structure change in sf crs. (Fixes
breakage in eixport).

* Fixed CRAN NOTE about not using geometry import. 

* Fix deprecated usage of tibble. 
 1) Internal sf objects now have "list" in the class of the sfc column. 
 2) No matrix sub-assignment into columns of a tibble. See #105. 
 
* New models `TRI0` and `PATH0` as analogues to SC0. 

* Fixed off-by-one error in segment colours when plotting ARC. Fixes #101. 

* Fixed unused geometry package import. 


# silicate 0.2.0

* Clarified Description and cleaned up examples, thanks to CRAN feedback. 

* First viable release. 

* Commited to ARC only being relevant to polygon layers. 

* New models `TRI0` and `PATH0` as analogues to SC0. 

# silicate 0.1.1.9001

* `sc_uid` is now 6L characters by default, controlled by `uid_nchar` argument, and settable in option
`silicate.uid.size`. If `option(silicate.uid.type = "integer")` this is ignored, but that is considered experimental for now. 

* Models now have print methods. 

* The  `filter` method for SC is now exported. 

* Incorporate new version of `SC0` model, a better starting point for decomposing objects. 

* SC can now interpret TRI, which allows for very easy DEL re-triangulations and fine
 control over the quality and details of the mesh
 
* TRI now allows linear topologies, they are assumed to be all island paths (see `anglr::DEL` for a possible 
better approach). 

* SC and PATH now allow degenerate paths and edges, with plot as pixel and point respectively. 

* TRI model now includes object_link_triangle, and drops object_ and path_ from triangle table to
 be consistent with anglr::DEL. 
 
* The meta table is now always present in models. 

* Migrated from JS rearcut to C++ decido for triangulation.

* Cleaned up the triangulation logic. 


# silicate 0.1.0

* added functions compact_indexes and expand_indexes to remove 
 and restore the unique labels on entities - these are purely 
 to explore what the minimal size of a model might be 
 
* more consistency for sc_verbs, sc_coord, sc_vertex, sc_object,
 sc_node, sc_arc, sc_path, sc_uid
 
* established models SC, PATH, ARC, TRI

* removed PRIMITIVE

* (breaking) `sc_uid` now generic, with argument `x` (not `n`)

* various fixes in line with gibble 0.0.1.9002 to properly support sp structures

* fixed bug where a single-path data set exposed an offset assumption applied
 by sc_segment, fixes https://github.com/hypertidy/silicate/issues/40
 
* fixed bug where 0-column dataframe was assumed to be a list of vectors
 https://github.com/hypertidy/anglr/issues/55
 
* added `polymesh` data

* support for trip

* install flight_tracks example

* support for sp!

* moved sf specific facility from scsf

* renamed package

* `sc_rand` is deprecated, to be replaced by `sc_uid`

# silicate 0.0.2

* (was called sc)
* cleaned up the un-join with a specific function from 'unjoin' package
* added internal function `sc_edge`, and exported `sc_node` and renamed arc_node to `NARC`
* removed all sf-specific facilities to external package
* revert to sf 0.2-7 to enable vignette migration
* PATH model added (with ongoing migration from "branch" term to "path" term)
* BRANCH model effectively removed, replaced by the PATH model
* PRIMITIVES model added, including bare-nodes arc_node function to return the 3-way nodes


# silicate 0.0.1

* (was called sc)
* first functions, the BRANCH model for sf



