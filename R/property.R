extract_property <- function(x, name, extra_class = NULL) {
   pattern <- sprintf("^%s\\.(.*)$", gsub("\\.", "\\\\.", name))
   x <- x[, grep(pattern, colnames(x)), drop = FALSE]
   colnames(x) <- sub(pattern, "\\1", colnames(x))
   class(x) <- c(extra_class, "data.frame")
   x
}
