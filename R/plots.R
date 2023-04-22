vr_content_score_evplot <- function(vsx, font_size = 6) {
    vr_score_evplot(structure(list(meta = vsx$meta, plays = vsx$x), class = "datavolley"), with_summary = vsx$plot_summary, markers = vsx$plot_markers, home_colour = vsx$css$header_background, visiting_colour = vsx$css$header_background, font_size = font_size)
}

#' @rdname vr_score_evplot
#' @export
vr_plot_markers <- function() {
    tribble(~marker_event, ~icon_name, ~unicode,
            "ace", "burst", "\ue4dc", ## or "bolt-lightning", "\ue0b7",
            "block", "hand", "\uf256",
            "error", "triangle-exclamation", "\uf071")
}

get_plot_marker <- function(event, markers, as = "unicode") {
    ## `as` can be "unicode" or "icon_name" or "svg"
    this <- markers[which(markers$marker_event == event), ]
    if (nrow(this) != 1) return("")
    switch(as,
           unicode =,
           icon_name = this[[as]],
           svg = as.character(fontawesome::fa(this$icon_name)),
           "")
}

#' Score evolution plot
#'
#' @param x datavolley or string: as returned by \code{datavolley::dv_read}, or the path to such a file
#' @param with_summary logical: if `TRUE`, show team summary statistics. For beach, this is by end; for indoor summaries will be by set
#' @param markers logical or data.frame: either `FALSE` for no markers, `TRUE` to use the markers specified by [vr_plot_markers()], or a data.frame as returned by [vr_plot_markers()]
#' @param home_colour string: colour for home team
#' @param visiting_colour string: colour for visiting team
#' @param low_colour,mid_colour,high_colour string: colours for low, mid, and high performance (used for `with_summary` only)
#' @param font_size scalar: font size
#'
#' @return A ggplot object
#'
#' @examples
#' vr_score_evplot(dv_example_file())
#'
#' @export
vr_score_evplot <- function(x, with_summary = FALSE, markers = FALSE, home_colour = "darkblue", visiting_colour = "darkred", low_colour = "#800000", mid_colour = "#202020", high_colour = "#008000", font_size = 12) {
    if (is.data.frame(markers)) {
        use_markers <- TRUE
    } else {
        use_markers <- isTRUE(markers)
        if (use_markers) markers <- vr_plot_markers()
    }
    if (is.string(x) && file.exists(x) && grepl("\\.dvw$", x, ignore.case = TRUE)) {
        x <- datavolley::dv_read(x, skill_evaluation_decode = "guess")
    }
    assert_that(inherits(x, c("datavolley", "peranavolley")))
    px <- datavolley::plays(x)
    file_type <- x$file_meta$file_type
    if (is.null(file_type) || !file_type %in% c("indoor", "beach", "perana_indoor", "perana_beach")) file_type <- guess_data_type(px)
    beach <- grepl("beach", file_type)
    sc <- px %>% group_by(.data$point_id) %>% dplyr::slice_tail(n = 1) %>% ungroup %>%
        dplyr::select("point_id", "set_number", "home_team", "home_team_score", "visiting_team", "visiting_team_score") %>% distinct %>% na.omit() %>%
        mutate(ok = lead(.data$home_team_score) != .data$home_team_score | lead(.data$visiting_team_score) != .data$visiting_team_score)
    sc$ok[nrow(sc)] <- TRUE ## filter on `ok` to discard e.g. timeouts, subs, and other non-score-change events
    sc <- sc %>% dplyr::filter(.data$ok) %>% mutate(pid = dplyr::row_number(), diff = .data$home_team_score - .data$visiting_team_score, teamcolor = case_when(.data$diff < 0 ~ visiting_colour, TRUE ~ home_colour)) %>% dplyr::select(-"ok")
    if (nrow(sc) < 2) return(NULL)
    ## markers for blocks, aces, errors
    if (use_markers) {
        sc <- left_join(sc, px %>% mutate(evaluation = case_when(.data$skill %in% c("Serve", "Attack", "Set", "Freeball") & .data$evaluation == "Error" ~ "Unforced error",
                                                                 .data$skill == "Block" & .data$evaluation == "Invasion" ~ "Unforced error",
                                                                 TRUE ~ .data$evaluation)) %>%
                            dplyr::filter(.data$evaluation %in% c("Ace", "Winning block", "Unforced error")) %>%
                            mutate(marker_team = if_else(.data$home_team == .data$team, "home", "visiting")) %>%
                            dplyr::select("point_id", marker_event = "evaluation", "marker_team"), by = "point_id")
    }
    setx <- c(0, sc$pid[which(diff(sc$set_number) > 0)]) + 0.5
    sc <- mutate(sc, set_number = paste0("Set ", .data$set_number))
    yr <- c(min(-4, min(sc$diff, na.rm = TRUE)), max(4, max(sc$diff, na.rm = TRUE))) ## y-range, at least -4 to +4
    yr0 <- c(min(sc$diff, na.rm = TRUE), max(sc$diff, na.rm = TRUE)) ## data y-range
    blockx <- smx <- NULL
    if (isTRUE(with_summary)) {
        if (beach) {
            px <- mutate(px, score_tot = .data$home_team_score + .data$visiting_team_score,
                         block = case_when(.data$set_number < 3 ~ floor((.data$score_tot - 1) / 7) + 1L,
                                           TRUE ~ floor((.data$score_tot - 1) / 5) + 1L),
                         block = as.integer(as.factor(paste0("S", .data$set_number, "E", .data$block)))) ## block within set
        } else {
            px$block <- px$set_number ## todo later, by score splits
        }
        ## calculate summary stats by block and team
        smx <- px %>% dplyr::filter(!is.na(.data$block), !is.na(.data$team)) %>%
            mutate(team_hv = if_else(.data$home_team == .data$team, "home", "visiting"),
                   opp_team_hv = if_else(.data$team_hv == "home", "visiting", "home")) %>%
            group_by(.data$block) %>%
            mutate(npts = length(unique(na.omit(.data$point_id))), ## how many points in the block
                   point_id0 = intersect(.data$point_id, sc$point_id)[1]) ## first point in the block
        ## if the last block in a set is short, merge it with the previous one
        smx <- smx %>% group_by(.data$set_number) %>% mutate(block_is_last = .data$block == max(.data$block, na.rm = TRUE),
                                                             min_block_in_set = min(.data$block, na.rm = TRUE)) %>% ungroup %>%
                       mutate(block = if_else (.data$npts < 5 & .data$block_is_last & .data$block > .data$min_block_in_set, .data$block - 1L, .data$block)) %>%
                       dplyr::select(-"block_is_last", -"min_block_in_set")
        smx <- left_join(smx %>% group_by(.data$block, .data$team_hv) %>%
                         dplyr::summarize(err_opps = length(unique(na.omit(.data$point_id[!is.na(.data$skill)]))), ## number of opportunities (rallies) we had to make errors
                                          point_id = .data$point_id0[1], npts = .data$npts[1],
                                          `Nsrv` = sum(.data$skill == "Serve", na.rm = TRUE),
                                          Ace = sum(.data$evaluation == "Ace", na.rm = TRUE),
                                          `BP` = as.character(prc(round(sum(.data$skill == "Serve" & .data$point_won_by == .data$team, na.rm = TRUE) %/n/% sum(.data$skill == "Serve", na.rm = TRUE) * 100))),
                                          `BPw` = sum(.data$skill == "Serve" & .data$point_won_by == .data$team, na.rm = TRUE),
                                          `Nrec` = sum(.data$skill == "Reception", na.rm = TRUE),
                                          `modSO` = as.character(prc(round(sum(.data$skill == "Reception" & .data$point_won_by == .data$team, na.rm = TRUE) %/n/% sum(.data$skill == "Reception", na.rm = TRUE) * 100))),
                                          `Natt` = sum(.data$skill == "Attack", na.rm = TRUE),
                                          `K` = as.character(prc(round(sum(.data$skill == "Attack" & .data$evaluation == "Winning attack", na.rm = TRUE) %/n/% sum(.data$skill == "Attack", na.rm = TRUE) * 100))),
                                          Blk = sum(.data$evaluation == "Winning block", na.rm = TRUE),
                                          attw = sum(.data$skill == "Attack" & .data$evaluation == "Winning attack", na.rm = TRUE),
                                          errs = sum(.data$skill %in% c("Serve", "Attack", "Set", "Freeball") & .data$evaluation == "Error", na.rm = TRUE) + sum(.data$skill == "Block" & .data$evaluation == "Invasion", na.rm = TRUE)) %>%
                         ungroup,
                         smx %>% group_by(.data$block, .data$opp_team_hv) %>%
                         dplyr::summarize(Nsrv_opp = sum(.data$skill == "Serve", na.rm = TRUE),
                                          Natt_opp = sum(.data$skill == "Attack" & .data$evaluation != "Error", na.rm = TRUE),
                                          SOw = sum(.data$skill == "Serve" & .data$point_won_by != .data$team, na.rm = TRUE)) %>%
                         ungroup %>% dplyr::rename(team_hv = "opp_team_hv"),
                         by = c("block", "team_hv"))
        smx <- smx %>% left_join(sc %>% dplyr::distinct(.data$point_id, .data$pid), by = "point_id") ## so we can plot by pid
        blockx <- unique(smx$pid) - 0.5 ## -0.5 so that the vertical line aligns with the left edge of the corresponding bar
    }
    p <- ggplot(sc, aes(.data$pid, .data$diff)) + ggplot2::theme_minimal(base_size = font_size) +
        ggplot2::geom_vline(xintercept = setx, col = "black", alpha = 0.5, size = 0.25) +
        ggplot2::geom_hline(yintercept = 0, col = "black", alpha = 0.5, size = 0.25) +
        ggplot2::geom_col(aes(fill = .data$teamcolor), width = 1.0, col = NA)
    if (!is.null(blockx)) p <- p + ggplot2::geom_vline(xintercept = setdiff(blockx, setx), col = "#555555", linetype = "dashed", alpha = 0.5, size = 0.25)
    p <- p + ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"), axis.text.x = ggplot2::element_text(hjust = 0))
    sets2 <- length(unique(na.omit(px$set_number))) < 3 ## only 2 sets
    if (!is.null(smx)) {
        sysfonts::font_add("condensedfont",
                           regular = system.file("extdata/fonts/RobotoCondensed-Light.ttf", package = "volleyreport", mustWork = TRUE),
                           bold = system.file("extdata/fonts/RobotoCondensed-Regular.ttf", package = "volleyreport", mustWork = TRUE),
                           italic = system.file("extdata/fonts/RobotoCondensed-LightItalic.ttf", package = "volleyreport", mustWork = TRUE),
                           bolditalic = system.file("extdata/fonts/RobotoCondensed-Italic.ttf", package = "volleyreport", mustWork = TRUE))
        thinspc <- intToUtf8(2^13 + 6) ## thin space, or +10 is thinner
        slash <- "\u200b/" ## if we use "/" characters in fractions, pandoc tries to convert e.g. 1/4 to the unicode one-quarter character, which we don't want. So insert a zero-width space ("\u200b") as a workaround
        for (tm in c("home", "visiting")) {
            for (thispid in unique(smx$pid[smx$team_hv == tm])) {
                tbx <- smx %>% dplyr::filter(.data$pid == thispid, .data$team_hv == tm)
                frac2col <- function(n, N, side = 0L, lthresh = 0.4, uthresh = 1 - lthresh, cl_thr = 0.8, low = low_colour, mid = mid_colour, high = high_colour) {
                    ## function to colour text, so we can highlight particularly good or poor performance in blocks
                    ## side = 0 => low is bad, high is good;
                    ##       -1 => higher is bad but ignore lower (i.e. errors)
                    ##        1 => higher is good but ignore lower (i.e. aces, blocks)
                    ## thresh is performance threshold, used for N > 3
                    ## cl_thresh is confidence limit threshold
                    if (N == 3) {
                        if ((n < 1 && side == 0) || (n > 2 && side < 0)) return(low) else if (n > 2 && side > -1) return(high)
                    } else if (N < 4) {
                        return(mid)
                    }
                    cl <- diff(suppressWarnings(prop.test(n, N)$conf.int))
                    if (((n/N < lthresh && side == 0L) || (n/N > uthresh && side < 0)) && cl < cl_thr) {
                        low
                    } else if (n/N > uthresh && side > -1 && cl < cl_thr) {
                        high
                    } else {
                        mid
                    }
                }
                txt <- paste0("<span style=\"color:", frac2col(tbx$BPw, tbx$Nsrv), ";\">BP", thinspc, tbx$BPw, slash, tbx$Nsrv, "</span>",
                              if (sets2) ", " else "<br />",
                              "<span style=\"color:", frac2col(tbx$SOw, tbx$Nsrv_opp), ";\">SO", thinspc, tbx$SOw, slash, tbx$Nsrv_opp,
                              "<br />",
                              "<span style=\"color:", frac2col(tbx$attw, tbx$Natt, lthresh = 0.3, uthresh = 0.6), ";\">Att", thinspc, tbx$attw, slash, tbx$Natt,
                              if (sets2) ", " else "<br />",
                              "<span style=\"color:", frac2col(tbx$Ace, tbx$Nsrv, side = 1L, uthresh = 0.3), ";\">Ace", thinspc, tbx$Ace, slash, tbx$Nsrv,
                              "<br />",
                              "<span style=\"color:", frac2col(tbx$Blk, tbx$Natt_opp, side = 1L, uthresh = 0.25), ";\">Blk", thinspc, tbx$Blk, slash, tbx$Natt_opp,
                              if (sets2) ", " else "<br />",
                              "<span style=\"color:", frac2col(tbx$errs, tbx$err_opps, side = -1L, uthresh = 0.35), ";\">Err", thinspc, tbx$errs) ## slash, tbx$err_opps,
                p <- p + ggplot2::annotate(GeomRichText, label = txt, x = thispid - 0.3, y = if (tm == "visiting") yr0[1] - 1.5 else yr0[2] + 1.5,
                                           hjust = 0, vjust = if (tm == "visiting") "top" else "bottom", size = 2, lineheight = 1.0, family = "condensedfont",
                                           fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) ## remove background and outline, no padding
            }
        }
        yr <- c(min(yr[1], yr0[1] - 6L - (!sets2) * 2), max(yr[2], yr0[2] + 6L + (!sets2) * 2))
    }
    if (use_markers && !all(is.na(sc$marker_event))) {
        sysfonts::font_add("fa6s", regular = system.file("fontawesome/webfonts/fa-solid-900.ttf", package = "fontawesome", mustWork = TRUE))
        ex <- sc %>% dplyr::filter(!is.na(.data$marker_event)) %>%
            mutate(marker_icon = case_when(.data$marker_event == "Ace" ~ get_plot_marker("ace", markers),
                                           .data$marker_event == "Winning block" ~ get_plot_marker("block", markers),
                                           .data$marker_event == "Unforced error" ~ get_plot_marker("error", markers)),
                   iy = case_when(.data$marker_team == "home" & .data$diff > 0 & .data$diff < max(.data$diff, na.rm = TRUE) ~ .data$diff + 1,
                                  .data$marker_team == "home" & .data$diff > 0 ~ .data$diff + 0.5,
                                  .data$marker_team == "home" ~ 1,
                                  .data$marker_team == "visiting" & .data$diff < 0 & .data$diff > min(.data$diff, na.rm = TRUE) ~ .data$diff - 1,
                                  .data$marker_team == "visiting" & .data$diff < 0 ~ .data$diff - 0.5,
                                  .data$marker_team == "visiting" ~ -1)) %>%
            dplyr::filter(!is.na(.data$marker_icon))
        p <- p + ggplot2::geom_text(data = ex, aes(y = .data$iy, label = .data$marker_icon, colour = .data$marker_event), family = "fa6s", size = 1.75) +
            ggplot2::scale_colour_manual(values = c(high_colour, low_colour, high_colour), guide = "none")
    }
    p <- p + ggplot2::scale_fill_manual(values = c(home_colour, visiting_colour), guide = "none") + ggplot2::labs(x = NULL, y = "Score\ndifference") +
        ggplot2::annotate(geom = "text", label = datavolley::home_team(x), x = 1, y = if (!is.null(smx)) yr[2] else diff(yr) * 0.9 + yr[1], hjust = 0, size = font_size * 0.35278, fontface = "bold") +
        ggplot2::annotate(geom = "text", label = datavolley::visiting_team(x), x = 1, y = if (!is.null(smx)) yr[1] else diff(yr) * 0.1 + yr[1], hjust = 0, size = font_size * 0.35278, fontface = "bold") +
        ggplot2::scale_x_continuous(labels = paste0("Set ", seq_along(setx)), breaks = setx, minor_breaks = NULL, expand = c(0.005, 0.005)) +
        ggplot2::scale_y_continuous(breaks = function(z) c(rev(seq(0, yr[1], by = -4)), seq(0, yr[2], by = 4)[-1]), limits = yr, labels = abs)
    p
}

