#' \pkg{volleyreport}
#'
#' Reporting of volleyball statistics
#'
#' @name volleyreport
#' @docType package
#' @importFrom assertthat assert_that is.flag is.string
#' @importFrom datavolley dv_example_file dv_flip_x dv_flip_y dv_xy ggcourt
#' @importFrom dplyr `%>%` across add_row bind_rows case_when count .data distinct group_by if_else lag lead full_join left_join mutate mutate_all n na_if slice summarize tibble tribble ungroup
#' @importFrom ggplot2 aes annotate element_rect element_text facet_wrap geom_col geom_hline geom_label geom_path geom_point geom_raster geom_segment geom_text geom_tile geom_vline ggplot ggtitle labs scale_colour_manual scale_fill_distiller scale_fill_gradient scale_fill_gradientn scale_fill_manual scale_linewidth scale_x_continuous scale_y_continuous theme theme_bw theme_minimal theme_void
#' @importFrom ggtext GeomRichText
#' @importFrom grDevices hcl.colors
#' @importFrom kableExtra add_header_above cell_spec column_spec kable_styling row_spec
#' @importFrom knitr kable
#' @importFrom methods as
#' @importFrom rmarkdown render
#' @importFrom stats prop.test na.omit setNames
#' @importFrom stringr str_to_title
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tidyselect where
#' @importFrom utils adist tail
NULL
