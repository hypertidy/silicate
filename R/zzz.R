# from: https://github.com/r-spatial/sf/blob/2d4a08d5e18ad3a0ca839b509383bb2cca77125a/R/tidyverse.R#L373
#  "2018-10-30 23:36:40 UTC"
#nocov start
# register_s3_method <- function(pkg, generic, class, fun = NULL) {
#   stopifnot(is.character(pkg), length(pkg) == 1)
#   stopifnot(is.character(generic), length(generic) == 1)
#   stopifnot(is.character(class), length(class) == 1)
#
#   if (is.null(fun)) {
#     fun <- get(paste0(generic, ".", class), envir = parent.frame())
#   } else {
#     stopifnot(is.function(fun))
#   }
#
#   if (pkg %in% loadedNamespaces()) {
#     registerS3method(generic, class, fun, envir = asNamespace(pkg))
#   }
#
#   # Always register hook in case package is later unloaded & reloaded
#   setHook(
#     packageEvent(pkg, "onLoad"),
#     function(...) {
#       registerS3method(generic, class, fun, envir = asNamespace(pkg))
#     }
#   )
# }
# # nocov end
#
 .onLoad <- function(...) {
   #register_s3_method("dplyr", "filter", "SC")
   op <- getOption("silicate.uid.type")

   if (is.null(op) || is.na(op) || length(op) < 1) {
     op <- "uuid"  ## or "integer"
     options(silicate.uid.type = op)
   }
   op.size <- getOption("silicate.uid.size")
   if (is.null(op.size) || is.na(op.size) || length(op.size) < 1) {

      options(silicate.uid.size = 6L)
   }
   invisible()
 }


