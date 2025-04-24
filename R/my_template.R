#' Create a new document using My Template
#' @export
#' my_template

my_template <- function() {
  rmarkdown::draft("my_document.Rmd", template = "Fieldnotes", package = "DFOfieldnotes")
}

