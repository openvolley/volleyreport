#' \pkg{volleyreport}
#'
#' Reporting of volleyball statistics
#'
#' @name volleyreport
#' @docType package
#' @importFrom assertthat assert_that is.flag is.string
#' @importFrom datavolley dv_example_file
#' @importFrom dplyr `%>%` add_row bind_rows case_when .data distinct group_by lag lead left_join mutate mutate_all n na_if slice summarize tibble ungroup
#' @importFrom ggplot2 aes_string geom_path geom_point ggplot theme_bw theme_void
#' @importFrom kableExtra add_header_above cell_spec column_spec kable_styling row_spec
#' @importFrom knitr kable
#' @importFrom methods as
#' @importFrom rmarkdown render
#' @importFrom stats na.omit setNames
#' @importFrom stringr str_to_title
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils tail
NULL
