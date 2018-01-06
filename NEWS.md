# silicate dev

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

# sc 0.0.2

* cleaned up the un-join with a specific function from 'unjoin' package
* added internal function `sc_edge`, and exported `sc_node` and renamed arc_node to `NARC`
* removed all sf-specific facilities to external package
* revert to sf 0.2-7 to enable vignette migration
* PATH model added (with ongoing migration from "branch" term to "path" term)
* BRANCH model effectively removed, replaced by the PATH model
* PRIMITIVES model added, including bare-nodes arc_node function to return the 3-way nodes


# sc 0.0.1

* first functions, the BRANCH model for sf



