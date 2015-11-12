build_tags <- function(x) {
   if (is.data.frame(x)) {
      x
   } else if (is.vector(x)) {
      data.frame(tagName = names(x), tagValue = as.character(x))
   }
}
