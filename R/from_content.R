from_content <- function(x, extra_class = NULL, force_data_frame = TRUE) {
   is_object <- grepl("^\\{", trimws(x, "left"))
   if (is_object & force_data_frame) {
      x <- sprintf("[%s]", x)
   }
   result <- jsonlite::fromJSON(x, flatten = force_data_frame)
   class(result) <- c(extra_class, class(result))
   result
}
