vr_content_score_evplot <- function(vsx, font_size = 6) {
    vr_score_evplot(structure(list(meta = vsx$meta, plays = vsx$x), class = "datavolley"), home_colour = vsx$css$header_background, visiting_colour = vsx$css$header_background, font_size = font_size)
}

#' Score evolution plot
#'
#' @param x datavolley or string: as returned by \code{datavolley::dv_read}, or the path to such a file
#' @param home_colour string: colour for home team
#' @param visiting_colour string: colour for visiting team
#' @param font_size scalar: font size
#'
#' @return A ggplot object
#'
#' @examples
#' vr_score_evplot(dv_example_file())
#'
#' @export
vr_score_evplot <- function(x, home_colour = "darkblue", visiting_colour = "darkred", font_size = 12) {
    if (is.string(x) && file.exists(x) && grepl("\\.dvw$", x, ignore.case = TRUE)) {
        x <- datavolley::dv_read(x, skill_evaluation_decode = "guess")
    }
    assert_that(inherits(x, c("datavolley", "peranavolley")))
    sc <- datavolley::plays(x) %>% dplyr::filter(.data$point) %>% dplyr::select("set_number", "home_team", "home_team_score", "visiting_team", "visiting_team_score") %>% distinct %>% na.omit() %>%
        mutate(pid = dplyr::row_number(), diff = .data$home_team_score - .data$visiting_team_score,
               teamcolor = case_when(.data$diff < 0 ~ visiting_colour, TRUE ~ home_colour))
    if (nrow(sc) < 1) return(NULL)
    setx <- c(0, sc$pid[which(diff(sc$set_number) > 0)]) + 0.5
    sc <- mutate(sc, set_number = paste0("Set ", .data$set_number))
    yr <- c(min(-4, min(sc$diff, na.rm = TRUE)), max(4, max(sc$diff, na.rm = TRUE))) ## y-range
    ggplot(sc, aes_string("pid", "diff")) + ggplot2::theme_minimal(base_size = font_size) +
        ggplot2::geom_vline(xintercept = setx, col = "black", alpha = 0.5) +
        ggplot2::geom_col(aes_string(fill = "teamcolor"), width = 1.0, col = NA) +
        ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"), axis.text.x = ggplot2::element_text(hjust = 0)) +
        ggplot2::scale_fill_manual(values = c(home_colour, visiting_colour), guide = "none") +
        ggplot2::labs(x = NULL, y = "Score\ndifference") +
        ggplot2::annotate(geom = "text", label = datavolley::home_team(x), x = 1, y = diff(yr) * 0.9 + yr[1], hjust = 0, size = font_size * 0.35278, fontface = "bold") +
        ggplot2::annotate(geom = "text", label = datavolley::visiting_team(x), x = 1, y = diff(yr) * 0.1 + yr[1], hjust = 0, size = font_size * 0.35278, fontface = "bold") +
        ggplot2::scale_x_continuous(labels = paste0("Set ", seq_along(setx)), breaks = setx, minor_breaks = NULL, expand = c(0.005, 0.005)) +
        ggplot2::ylim(yr)
}



bbc_style <- function(font = "Helvetica") {
    ggplot2::theme(plot.title = ggplot2::element_text(family = font, size = 28, face = "bold", color="#222222"),
                                        #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
                 plot.subtitle = ggplot2::element_text(family=font,
                                                       size=22,
                                                       margin=ggplot2::margin(9,0,9,0)),
                 plot.caption = ggplot2::element_blank(),
                                        #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function

                                        #Legend format
                                        #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
                 legend.position = "top",
                 legend.text.align = 0,
                 legend.background = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank(),
                 legend.key = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(family=font,
                                                     size=18,
                                                     color="#222222"),

                                        #Axis format
                                        #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
                 axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(family=font,
                                                   size=18,
                                                   color="#222222"),
                 axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
                 axis.ticks = ggplot2::element_blank(),
                 axis.line = ggplot2::element_blank(),

                                        #Grid lines
                                        #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
                 panel.grid.major.x = ggplot2::element_blank(),

                                        #Blank background
                                        #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
                 panel.background = ggplot2::element_blank(),

                                        #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
                 strip.background = ggplot2::element_rect(fill="white"),
                 strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
             )
}
