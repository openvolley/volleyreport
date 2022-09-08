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
    yr <- c(min(-4, min(sc$diff, na.rm = TRUE)), max(4, max(sc$diff, na.rm = TRUE))) ## y-range, at least -4 to +4
    ggplot(sc, aes_string("pid", "diff")) + ggplot2::theme_minimal(base_size = font_size) +
        ggplot2::geom_vline(xintercept = setx, col = "black", alpha = 0.5, size = 0.25) +
        ggplot2::geom_hline(yintercept = 0, col = "black", alpha = 0.5, size = 0.25) +
        ggplot2::geom_col(aes_string(fill = "teamcolor"), width = 1.0, col = NA) +
        ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"), axis.text.x = ggplot2::element_text(hjust = 0)) +
        ggplot2::scale_fill_manual(values = c(home_colour, visiting_colour), guide = "none") +
        ggplot2::labs(x = NULL, y = "Score\ndifference") +
        ggplot2::annotate(geom = "text", label = datavolley::home_team(x), x = 1, y = diff(yr) * 0.9 + yr[1], hjust = 0, size = font_size * 0.35278, fontface = "bold") +
        ggplot2::annotate(geom = "text", label = datavolley::visiting_team(x), x = 1, y = diff(yr) * 0.1 + yr[1], hjust = 0, size = font_size * 0.35278, fontface = "bold") +
        ggplot2::scale_x_continuous(labels = paste0("Set ", seq_along(setx)), breaks = setx, minor_breaks = NULL, expand = c(0.005, 0.005)) +
        ggplot2::scale_y_continuous(breaks = function(z) c(rev(seq(0, yr[1], by = -4)), seq(0, yr[2], by = 4)[-1]), limits = yr)
}

