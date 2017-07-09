# rvest read_html doesn't correctly handle <br>, e.g read_html('x<br>y') %>%
# html_text results in xy; the issue has been brought up, but for some reason
# this is seen as an 'enhancement' though it is clearly a bug, as there is no use for the current

# The following comes from rentrop (thanks!)
# https://github.com/hadley/rvest/issues/175


html_text_collapse <- function(x, trim = FALSE, collapse = "\n"){
  UseMethod("html_text_collapse")
}

html_text_collapse.xml_nodeset <- function(x, trim = FALSE, collapse = "\n"){
  vapply(x, html_text_collapse.xml_node, character(1), trim = trim, collapse = collapse)
}

html_text_collapse.xml_node <- function(x, trim = FALSE, collapse = "\n"){
  paste(xml2::xml_find_all(x, ".//text()"), collapse = collapse)
}