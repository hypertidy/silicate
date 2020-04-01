#' Build sf from silicate
#'
#' Build sf
#' @param x silicate model
#'
#' @return sf dataframe
#' @export
#' @importFrom sfbuild to_sf
#' @export to_sf
#' @method to_sf sc
#' @aliases to_sf
#' @examples
#' ## PATHs are MULTILINESTRING, nominate LINESTRING and ignore object_
#' to_sf(PATH(minimal_mesh))
#' to_sf(PATH0(minimal_mesh))
#'
#' to_sf(ARC(minimal_mesh))
to_sf.sc <- function(x, sfc_type = NULL, geom_type = NULL) {
  df <- sc_coord(x)
  names(df) <- gsub("_", "", names(df))
  path <- sc_path(x)
  ncoord <- path[["ncoords_"]]

  sfc_type <- unique(path[["type"]]) %||% "LINESTRING"
  if (grepl("^MULTI", sfc_type)) sfc_type <- "MULTILINESTRING"
  stopifnot(length(sfc_type) == 1L)
  if (sfc_type == "LINESTRING") {
    # df[["multilinestring_id"]] <-
    #   rep(as.integer(factor(path[["object_"]])), ncoord)
      df[["linestring_id"]] <-
      rep(as.integer(factor(path[["path_"]])), ncoord)
     df <- dplyr::arrange(df,
                          #.data$multilinestring_id,
                              .data$linestring_id)
  }
  if (sfc_type == "MULTILINESTRING") {
     df[["multilinestring_id"]] <-
       rep(as.integer(factor(path[["object_"]])), ncoord)
    df[["linestring_id"]] <-
      rep(as.integer(factor(path[["path_"]])), ncoord)
    df <- dplyr::arrange(df,
                         .data$multilinestring_id,
                         .data$linestring_id)
  }
  # sfc_type <- sfc_type %||%  unique(path$type)
  # stopifnot(length(sfc_type) == 1L)
  # if (sfc_type == "MULTIPOLYGON") {
  #   df[["multipolygon_id"]] <-
  #     rep(as.integer(factor(path[["object_"]])), ncoord)
  #   df[["polygon_id"]] <-
  #     rep(as.integer(factor(path[["subobject"]])), ncoord)
  #   df[["linestring_id"]] <-
  #     rep(as.integer(factor(path[["path_"]])), ncoord)
  # }
  sfbuild::to_sf(df)
}
