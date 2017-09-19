#' @export
sc_coord.ltraj <- function(x, ...) {
  tibble::as_tibble(cbind(do.call("rbind", lapply(x, function(a) a[c("x", "y", "date")])), 
        do.call("rbind", lapply(x, function(a) attr(a, "infolocs")))))
}

#' @export
sc_path.ltraj <- function(x, ...) {
  ## gibble not really needed
  out <- tibble::as_tibble(cbind(nrow = unlist(lapply(x, nrow)), ncol = 3L))
  out[["type"]] <- "ltraj"
  out
}

