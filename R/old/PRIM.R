# ##Generate a PRIMITIVES model. 
# ##
# ##A PRIMITIVES model is a decomposition of spatial forms to primitives. 
# ##@param x input object
# ##@param ... arguments passed to methods
# ##@name PRIMITIVE
# ##@export
# PRIMITIVE <- function(x, ...) UseMethod("PRIMITIVE")
# ##@name PRIMITIVE
# ##@export
# PRIMITIVE.PATH <- function(x, ...) {
#   x$segment <- sc_segment(x, ...)
#   class(x) <- c("PRIMITIVE", class(x))
#   x
# }
# ##@name PRIMITIVE
# ##@export
# PRIMITIVE.default <- function(x, ...) {
#   x <- PATH(x) %>% PRIMITIVE(...)
#   class(x) <-  c("PRIMITIVE", class(x))
#   x
# }