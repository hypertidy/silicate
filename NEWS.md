# silicate dev

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



