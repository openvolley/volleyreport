vr_content_score_evplot <- function(vsx, font_size = 6) {
    vr_score_evplot(structure(list(meta = vsx$meta, plays = vsx$x), class = "datavolley"), with_summary = vsx$plot_summary, icons = vsx$plot_icons, home_colour = vsx$css$header_background, visiting_colour = vsx$css$header_background, font_size = font_size)
}

#' @rdname vr_score_evplot
#' @export
vr_plot_icons <- function() {
    out <- tribble(~icon_event, ~icon_name, ~unicode, ~description,
                   "ace", "burst", "\ue4dc", "Ace", ## or "bolt-lightning", "\ue0b7",
                   "kill", "star", "\uf005", "Attack kill",
                   "block", "hand", "\uf256", "Block kill",
                   "error", "triangle-exclamation", "\uf071", "Error",
                   "in_play", "circle", "\u6f", "In play")
    out$svg <- vapply(out$icon_name, function(nm) as.character(fontawesome::fa(nm)), FUN.VALUE = "", USE.NAMES = FALSE)
    out
}

get_plot_icon <- function(what, icons, as = "unicode") {
    if (!is.data.frame(icons)) return("")
    ## `as` can be "unicode" or "icon_name" or "svg" or "description"
    this <- icons[which(icons$icon_event == what), ]
    if (nrow(this) < 1) {
        ## did we pass the icon name rather than the event?
        this <- icons[which(icons$icon_name == what), ]
    }
    if (nrow(this) != 1) return("")
    this[[as]]
}

#' Score evolution plot
#'
#' @param x datavolley or string: as returned by \code{datavolley::dv_read}, or the path to such a file
#' @param with_summary logical: if `TRUE`, show team summary statistics. For beach, this is by end; for indoor summaries will be by set
#' @param icons logical or data.frame: either `FALSE` for no icons, `TRUE` to use the icons specified by [vr_plot_icons()], or a data.frame as returned by [vr_plot_icons()]. Note that only (free) fontawesome icons are supported
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
vr_score_evplot <- function(x, with_summary = FALSE, icons = FALSE, home_colour = "darkblue", visiting_colour = "darkred", low_colour = "#800000", mid_colour = "#202020", high_colour = "#008000", font_size = 12) {
    if (is.data.frame(icons)) {
        use_icons <- TRUE
    } else {
        use_icons <- isTRUE(icons)
        if (use_icons) icons <- vr_plot_icons()
    }
    if (is.string(x) && file.exists(x) && grepl("\\.(dvw|xml|vsm)$", x, ignore.case = TRUE)) {
        x <- datavolley::dv_read(x, skill_evaluation_decode = "guess")
    }
    assert_that(inherits(x, c("datavolley", "peranavolley")), msg = "x should be a datavolley object")
    px <- datavolley::plays(x)
    file_type <- x$file_meta$file_type
    if (is.null(file_type) || !file_type %in% c("indoor", "beach", "perana_indoor", "perana_beach")) file_type <- guess_data_type(px)
    beach <- grepl("beach", file_type)
    sc <- px %>% group_by(.data$point_id) %>% dplyr::slice_tail(n = 1) %>% ungroup %>%
        dplyr::select("point_id", "set_number", "home_team", "home_team_score", "home_score_start_of_point", "visiting_team", "visiting_team_score", "visiting_score_start_of_point") %>% distinct %>% na.omit() %>%
        mutate(ok = .data$home_score_start_of_point != .data$home_team_score | .data$visiting_score_start_of_point != .data$visiting_team_score)
    ## filter on `ok` to discard e.g. timeouts, subs, and other non-score-change events
    sc <- sc %>% dplyr::filter(.data$ok) %>% mutate(pid = dplyr::row_number(), diff = .data$home_team_score - .data$visiting_team_score, teamcolor = case_when(.data$diff < 0 ~ visiting_colour, TRUE ~ home_colour)) %>% dplyr::select(-"ok")
    if (nrow(sc) < 2) return(NULL)
    ## icons for blocks, aces, errors
    if (use_icons) {
        sc <- left_join(sc, px %>% mutate(evaluation = case_when(.data$skill %in% c("Serve", "Attack", "Set", "Freeball") & .data$evaluation == "Error" ~ "Unforced error",
                                                                 .data$skill == "Block" & .data$evaluation == "Invasion" ~ "Unforced error",
                                                                 TRUE ~ .data$evaluation)) %>%
                            dplyr::filter(.data$evaluation %in% c("Ace", "Winning block", "Unforced error")) %>%
                            mutate(icon_team = if_else(.data$home_team == .data$team, "home", "visiting")) %>%
                            dplyr::select("point_id", icon_event = "evaluation", "icon_team"), by = "point_id")
    }
    setx <- c(0, sc$pid[which(diff(sc$set_number) > 0)]) + 0.5
    sc <- mutate(sc, set_number = paste0("Set ", .data$set_number))
    yr <- c(min(-4, min(sc$diff, na.rm = TRUE)), max(4, max(sc$diff, na.rm = TRUE))) ## y-range, at least -4 to +4
    yr0 <- c(min(0, min(sc$diff, na.rm = TRUE)), max(0, max(sc$diff, na.rm = TRUE))) ## data y-range
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
    p <- ggplot(sc, aes(x = .data$pid, y = .data$diff)) + theme_minimal(base_size = font_size) +
        ggplot2::coord_cartesian(clip = "off") + ## to stop team names being clipped, see below
        geom_vline(xintercept = setx, col = "black", alpha = 0.5, size = 0.25) +
        geom_hline(yintercept = 0, col = "black", alpha = 0.5, size = 0.25) +
        geom_col(aes(fill = .data$teamcolor), width = 1.0, col = NA)
    if (!is.null(blockx)) p <- p + geom_vline(xintercept = setdiff(blockx, setx), col = "#555555", linetype = "dashed", alpha = 0.5, size = 0.25)
    p <- p + theme(strip.background = element_rect(fill = "white"), axis.text.x = element_text(hjust = 0))
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
                p <- p + annotate(GeomRichText, label = txt, x = thispid - 0.3, y = if (tm == "visiting") yr0[1] - 1.5 else yr0[2] + 1.5,
                                           hjust = 0, vjust = if (tm == "visiting") "top" else "bottom", size = 2, lineheight = 1.0, family = "condensedfont",
                                           fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) ## remove background and outline, no padding
            }
        }
        yr <- c(min(yr[1], yr0[1] - 6L - (!sets2) * 2), max(yr[2], yr0[2] + 6L + (!sets2) * 2))
    }
    icon_names <- character()
    if (use_icons && !all(is.na(sc$icon_event))) {
        sysfonts::font_add("fa6s", regular = system.file("fontawesome/webfonts/fa-solid-900.ttf", package = "fontawesome", mustWork = TRUE))
        ex <- sc %>% dplyr::filter(!is.na(.data$icon_event)) %>%
            ## convert icon_event entries to what we expect to see in the icons table
            mutate(icon_event = case_when(.data$icon_event == "Ace" ~ "ace",
                                          .data$icon_event == "Winning block" ~ "block",
                                          .data$icon_event == "Unforced error" ~ "error"),
                   iy = case_when(.data$icon_team == "home" & .data$diff > 0 & .data$diff < max(.data$diff, na.rm = TRUE) ~ .data$diff + 1,
                                  .data$icon_team == "home" & .data$diff > 0 ~ .data$diff + 0.5,
                                  .data$icon_team == "home" ~ 1,
                                  .data$icon_team == "visiting" & .data$diff < 0 & .data$diff > min(.data$diff, na.rm = TRUE) ~ .data$diff - 1,
                                  .data$icon_team == "visiting" & .data$diff < 0 ~ .data$diff - 0.5,
                                  .data$icon_team == "visiting" ~ -1)) %>%
            dplyr::filter(!is.na(.data$icon_event))
        ex <- left_join(ex, icons %>% dplyr::select("icon_event", "icon_name", icon = "unicode"), by = "icon_event")
        icon_names <- unique(na.omit(ex$icon_name))
        p <- p + geom_text(data = ex, aes(y = .data$iy, label = .data$icon, colour = .data$icon_event), family = "fa6s", size = 1.75) +
            scale_colour_manual(values = c(ace = "#008000", error = "#800000", block = "#008000"), guide = "none")
    }
    p <- p + scale_fill_manual(values = c(home_colour, visiting_colour), guide = "none") + labs(x = NULL, y = "Score\ndifference") +
        scale_x_continuous(labels = paste0("Set ", seq_along(setx)), breaks = setx, minor_breaks = NULL, expand = c(0.005, 0.005)) +
        scale_y_continuous(breaks = function(z) c(rev(seq(0, yr[1], by = -4)), seq(0, yr[2], by = 4)[-1]), limits = yr, labels = abs)
    ## team names, use linebreaks to shift leftwards of the y-axis, and rely on the margin to stop them being cropped
    p <- p + annotate(geom = "text", label = paste0(datavolley::home_team(x), "\n\n\n"), x = 0, y = yr[2], angle = 90, hjust = 1, vjust = 0, size = font_size * 0.35278, fontface = "bold") +
        annotate(geom = "text", label = paste0(datavolley::visiting_team(x), "\n\n\n"), x = 0, y = yr[1], angle = 90, hjust = 0, vjust = 0, size = font_size * 0.35278, fontface = "bold") +
        theme(plot.margin = ggplot2::margin(0, 0, 0, 2, unit = "lines"))
    if (length(icon_names) > 0) attr(p, "icon_names") <- icon_names
    p
}

#' Court plots
#'
#' @param x datavolley or string: as returned by \code{datavolley::dv_read}, or the path to such a file
# @param team string: team name or "home" or "visiting"
# @param font_size scalar: font size
# @param attack_plot_style string: style for the attack plot: either "zones", "subzones_heatmap", "coordinates_lines", "coordinates_heatmap", or "guess"
# @param icons logical or data.frame: either `FALSE` for no icons, `TRUE` to use the icons specified by [vr_plot_icons()], or a data.frame as returned by [vr_plot_icons()]. Note that only (free) fontawesome icons are supported
#' @param ... : passed to downstream functions
#'
#' @return A ggplot object
#'
#' @examples
#' vr_court_plots(dv_example_file())
#'
#' @export
vr_court_plots <- function(x, ...) {
    if (is.string(x) && file.exists(x) && grepl("\\.(dvw|xml|vsm)$", x, ignore.case = TRUE)) {
        x <- datavolley::dv_read(x, skill_evaluation_decode = "guess")
    }
    assert_that(inherits(x, c("datavolley", "peranavolley")), msg = "x should be a datavolley object")
    px <- datavolley::plays(x)
    dots <- list(...)
    if (!"attack_plot_style" %in% names(dots) || tolower(dots$attack_plot_style) %eq% "guess") {
        ## if plot style is guess, try and guess on the basis of the full data, otherwise if it happens plot by plot we can get different plot styles in the same report (for home compared to away team)
        ax <- dplyr::filter(px, .data$skill == "Attack")
        if (nrow(ax) > 0) dots$attack_plot_style <- guess_attack_plot_style(ax)
    }
    p1 <- do.call(vr_reception_plot, c(list(px, team = "home"), dots))
    p2 <- do.call(vr_attack_plot, c(list(px, team = "home"), dots))
    p3 <- do.call(vr_reception_plot, c(list(px, team = "visiting"), dots))
    p4 <- do.call(vr_attack_plot, c(list(px, team = "visiting"), dots))
    if (!is.null(p1) || !is.null(p2) || !is.null(p3) || !is.null(p4)) {
        getrsz <- function(z) { rh <- attr(z, "rel_size"); if (is.null(rh)) 1.0 else rh }
        p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 4, rel_widths = c(getrsz(p1), getrsz(p2), getrsz(p3), getrsz(p4)), align = "h", axis = "b")
        ## collect up any "icon_names" attributes from those plot objects and pass those back as the "icon_names" attribute of our returned plot
        ## this is a bit fragile but done so we can show (or not) the corresponding icons in the report legend
        icns <- c(attr(p1, "icon_names"), attr(p2, "icon_names"), attr(p3, "icon_names"), attr(p4, "icon_names"))
        if (length(icns) > 0) attr(p, "icon_names") <- icns
        p
    } else {
        NULL
    }
}

vr_reception_plot <- function(x, team = "home", font_size = 7, reception_plot_colour = vr_css()$header_background, ...) {
    assert_that(is.string(team), !is.na(team))
    beach <- grepl("beach", guess_data_type(x))
    target_team <- if (team == "home") datavolley::home_team(x) else if (team == "visiting") datavolley::visiting_team(x) else team
    rx <- x %>% dplyr::filter(!is.na(.data$start_zone), !is.na(.data$end_zone),
                               ((.data$team == target_team & .data$skill == "Reception") | (.data$team != target_team & .data$skill == "Serve")))
    if (nrow(rx) < 1) return(NULL)
    rx <- cbind(rx, dv_xy(rx$start_zone, end = "upper", xynames = c("sx", "sy"), as_for_serve = TRUE), dv_xy(rx$end_zone, end = "lower", xynames = c("rx", "ry"))) %>%
        mutate(ry = case_when(.data$skill == "Serve" & (.data$special_code == "Ball in net" | .data$end_zone %in% c(2, 3, 4)) ~ 3.5,
                              .data$skill == "Serve" & (.data$special_code == "Ball out - long" | .data$end_zone %in% c(5, 6, 1)) ~ 0.4,
                              .data$skill == "Serve" & (.data$special_code == "Ball out - right" | .data$end_zone %in% c(1, 9, 2)) ~ 3.6,
                              .data$skill == "Serve" & (.data$special_code == "Ball out - left" | .data$end_zone %in% c(4, 7, 5)) ~ 0.4,
                              TRUE ~ .data$ry))
    ## summarize serve start and end locations, to be drawn as line segments
    sx <- rx %>% count(.data$sx, .data$sy, .data$rx, .data$ry)
    ## not-quite-white-to-whatever colour map based on reception_plot_colour
    cpal <- tryCatch(tail(colorRampPalette(c("#FFFFFF", reception_plot_colour))(21), -4), error = function(e) rev(tail(hcl.colors(21, palette = "Blues"), -4)))
    val2col <- colorRamp(c(cpal[1], cpal[length(cpal)]))
    ## receptions, to be shown as tiles
    rx <- rx %>% dplyr::filter(.data$skill == "Reception") %>% ## exclude serve errors now
        group_by(.data$rx, .data$ry) %>%
        dplyr::summarize(N = n(), `SO %` = mean(.data$point_won_by == .data$team, na.rm = TRUE) * 100,
                         text = paste0("N=", .data$N, "\nSO=", round(.data$`SO %`), "%")) %>%
        ungroup %>%
        mutate(text_bw = white_or_black((.data$N - min(.data$N)) / (max(.data$N) - min(.data$N)), mapper = val2col))
    ggplot() + geom_tile(data = rx, aes(x = .data$rx, y = .data$ry, fill = .data$N), width = 1, height = 1) +
        ggcourt(labels = NULL, show_3m_line = !beach, label_font_size = font_size * 0.8, show_zones = FALSE,##font_size * 0.7,
                base_size = font_size, court = "lower", line_width = 0.3) + ## "full", ylim = c(0, 5)) +
        ##geom_segment(data = sx, aes(x = .data$sx, y = .data$sy, xend = .data$rx, yend = .data$ry, linewidth = .data$n), colour = "grey80",
        ##             arrow = grid::arrow(angle = 15, length = unit(2, "mm"), type = "closed")) +
        geom_text(data = rx %>% dplyr::filter(!.data$text_bw), aes(x = .data$rx, y = .data$ry, label = .data$text), size = 1.5, colour = "white") +
        geom_text(data = rx %>% dplyr::filter(.data$text_bw), aes(x = .data$rx, y = .data$ry, label = .data$text), size = 1.5, colour = "black") +
        ##scale_linewidth(guide = "none", range = c(0.5, 1)) +
        theme(legend.direction = "horizontal", legend.position = "top") +
        scale_fill_gradientn(colors = cpal, label = NULL, name = "N") +
        ggplot2::labs(caption = paste(strwrap(paste0(target_team, " reception"), 25, simplify = FALSE)[[1]], collapse = "\n")) + theme(plot.caption = element_text(hjust = 0.5))
}

## given value v that will be mapped to a fill colour via mapper, should we use white (FALSE) or black (TRUE) text?
## use very approximate perceptual lightness of colour
white_or_black <- function(v, mapper) apply(mapper(v), 1, function(z) (z[1] * 299 + z[2] * 587 + z[3] * 114) / 1000 / 255) > 0.7

guess_attack_plot_style <- function(ax) {
    conemiss <- mean(is.na(ax$end_cone))
    zmiss <- mean(is.na(ax$end_zone))
    szmiss <- mean(is.na(ax$end_subzone) | is.na(ax$end_zone))
    cmiss <- mean(is.na(ax$end_coordinate))
    if (cmiss <= (szmiss + 0.05) && cmiss < 0.2 * nrow(ax)) "coordinates_lines" else if (szmiss < zmiss && szmiss < 0.2 * nrow(ax)) "subzones_heatmap" else if (conemiss <= zmiss && conemiss < 0.2 * nrow(ax)) "cones" else "zones"
    ## note that szmiss can never be less than zmiss, so we default to a zone plot in prference to a subzone plot
}

vr_attack_plot <- function(x, team = "home", icons = vr_plot_icons(), attack_plot_style = "guess", attack_plot_colour = vr_css()$header_background, font_size = 7, ...) {
    assert_that(is.string(team), !is.na(team))
    attack_plot_style <- match.arg(attack_plot_style, c("guess", "zones", "cones", "subzones_heatmap", "coordinates_lines", "coordinates_heatmap"))
    beach <- grepl("beach", guess_data_type(x))
    target_team <- if (team == "home") datavolley::home_team(x) else if (team == "visiting") datavolley::visiting_team(x) else team
    ax <- x %>% dplyr::filter(.data$skill == "Attack", .data$team == target_team)
    if (nrow(ax) < 1) return(NULL)
    sysfonts::font_add("fa6s", regular = system.file("fontawesome/webfonts/fa-solid-900.ttf", package = "fontawesome", mustWork = TRUE))
    if (attack_plot_style == "guess") attack_plot_style <- guess_attack_plot_style(ax)
    ## white-to-whatever colour map based on attack_plot_colour
    cpalw <- tryCatch(colorRampPalette(c("#FFFFFF", attack_plot_colour))(21), error = function(e) rev(hcl.colors(21, palette = "Purples")))
    ## and not including white
    cpal <- tryCatch(tail(colorRampPalette(c("#FFFFFF", attack_plot_colour))(21), -4), error = function(e) rev(tail(hcl.colors(21, palette = "Purples"), -4)))
    val2col <- colorRamp(c(cpal[1], cpal[length(cpal)]))
    if (attack_plot_style == "zones") {
        ax <- ax %>% dplyr::filter(!is.na(.data$end_zone))
        if (nrow(ax) < 1) return(NULL)
        ax <- cbind(ax, dv_xy(ax$start_zone, end = "lower", xynames = c("sx", "sy")), dv_xy(ax$end_zone, end = "upper", xynames = c("ex", "ey")))
        if (beach) {
            ax <- mutate(ax, attack_side = case_when(.data$start_zone %in% c(4, 7, 5) ~ "From left",
                                                     .data$start_zone %in% c(3, 8, 6) ~ "From middle",
                                                     .data$start_zone %in% c(2, 9, 1) ~ "From right"),
                         attack_side = factor(.data$attack_side, levels = c("From left", "From middle", "From right")))
        } else {
            ## TODO change this to separate quick/slide/pipe from other
            ax <- mutate(ax, attack_side = case_when(.data$start_zone %in% c(4, 7, 5) ~ "From left",
                                                     .data$start_zone %in% c(3, 8, 6) ~ "From middle",
                                                     .data$start_zone %in% c(2, 9, 1) ~ "From right"),
                         attack_side = factor(.data$attack_side, levels = c("From left", "From middle", "From right")))
        }
        ax <- ax %>% group_by(.data$ex, .data$ey, .data$attack_side) %>%
            dplyr::summarize(N = n(), `Kill %` = mean(.data$point_won_by == .data$team, na.rm = TRUE) * 100,
                             text = paste0("N=", .data$N, "\nK=", round(.data$`Kill %`), "%")) %>%
            ungroup %>%
            mutate(text_bw = white_or_black((.data$N - min(.data$N)) / (max(.data$N) - min(.data$N)), mapper = val2col))
        p <- ggplot() + geom_tile(data = ax, aes(x = .data$ex, y = .data$ey, fill = .data$N)) +
            ggcourt(labels = NULL, show_3m_line = !beach, label_font_size = font_size * 0.8, show_zones = FALSE,
                    base_size = font_size, court = "upper", line_width = 0.3) +
            geom_text(data = ax %>% dplyr::filter(!.data$text_bw), aes(x = .data$ex, y = .data$ey, label = .data$text), size = 1.5, colour = "white") +
            geom_text(data = ax %>% dplyr::filter(.data$text_bw), aes(x = .data$ex, y = .data$ey, label = .data$text), size = 1.5, colour = "black") +
            scale_fill_gradientn(colors = cpal, labels = NULL, guide = "none") + ##theme(legend.direction = "horizontal", legend.position = "top") +
            facet_wrap(~attack_side, ncol = 2)
        attr(p, "rel_size") <- 1.0
    } else if (attack_plot_style == "cones") {
        ## these don't really work very well
        ##
        ## data for testing
        ##ax$end_cone <- sample.int(n = 7, size = nrow(ax), replace = TRUE); ax$end_cone[ax$start_zone %in% c(3, 8, 6) & runif(nrow(ax)) > 0.9] <- 8L
        ax <- ax %>% dplyr::filter(!is.na(.data$end_cone))
        if (nrow(ax) < 1) return(NULL)
        ## need to collapse start zones by side, so that we don't get overlapped cones
        ax <- mutate(ax, start_zone = case_when(.data$start_zone %in% c(4, 7, 5) ~ 4L,
                                                .data$start_zone %in% c(3, 8, 6) ~ 3L,
                                                .data$start_zone %in% c(2, 9, 1) ~ 2),
                     attack_side = case_when(.data$start_zone == 4 ~ "From left",
                                             .data$start_zone == 3 ~ "From middle",
                                             .data$start_zone == 2 ~ "From right"),
                     attack_side = factor(.data$attack_side, levels = c("From left", "From middle", "From right")))
        ## create data.frame for polygons
        px <- ax %>% count(.data$start_zone, .data$attack_side, .data$end_cone, name = "N")
        px <- bind_cols(px, dv_cone2xy(start_zones = px$start_zone, end_cones = px$end_cone, end = "upper", as = "polygons"))
        px <- tibble(x = unlist(px$ex), y = unlist(px$ey), id = rep(seq_len(nrow(px)), each = 4), N = rep(px$N, each = 4), attack_side = rep(px$attack_side, each = 4))
        ## and data.frame for labels
        ax <- cbind(ax, dv_xy(ax$start_zone, end = "lower", xynames = c("sx", "sy")),
                    dv_cone2xy(start_zones = ax$start_zone, end_cones = ax$end_cone, end = "upper", xynames = c("ex", "ey"), as = "points")) %>%
            mutate(sy = 3.5,
                   ## modify label y-position
                   ey = case_when(.data$attack_side == "From middle" & .data$end_cone %in% 2:6 ~ 5.4,
                                  .data$attack_side == "From middle" ~ 4.8,
                                  TRUE ~ .data$ey),
                   ## label angle
                   la = atan2(.data$ey - .data$sy, .data$ex - .data$sx) / pi * 180,
                   la = if_else(.data$la > 90, .data$la - 180, .data$la),
                   la = case_when(.data$attack_side == "From left" & .data$end_cone == 1 ~ 90,
                                  .data$attack_side == "From right" & .data$end_cone == 1 ~ -90,
                                  .data$attack_side == "From middle" ~ 90,
                                  TRUE ~ .data$la),
                   ## label justification
                   lhj = case_when(.data$attack_side == "From middle" & .data$end_cone %in% 2:6 ~ 0,
                                   .data$attack_side == "From middle" ~ 1,
                                   .data$attack_side == "From right" & .data$end_cone == 1 ~ -0.1,
                                   .data$attack_side == "From left" & .data$end_cone == 1 ~ 1.2,
                                   .data$la > 0 & (.data$ey > 4.5 | .data$ex > 3) ~ 1.2,
                                   .data$ex > 0.5 & .data$ex < 3.5 & .data$ey < 5 ~ 0.5,
                                   TRUE ~ -0.1))
        ax <- ax %>% group_by(.data$sx, .data$sy, .data$ex, .data$ey, .data$la, .data$lhj, .data$attack_side) %>%
            dplyr::summarize(N = n(), `Kill %` = mean(.data$point_won_by == .data$team, na.rm = TRUE) * 100,
                             text = paste0("N=", .data$N, if_else(.data$attack_side == "From middle", "\n", ","), "K=", round(.data$`Kill %`), "%")) %>% ungroup %>%
            mutate(text_bw = white_or_black((.data$N - min(.data$N)) / (max(.data$N) - min(.data$N)), mapper = val2col))

        p <- ggplot() + ggplot2::geom_polygon(data = px, aes(.data$x, .data$y, group = .data$id, fill = .data$N), colour = "grey80", linewidth = 0.3) +
            ggcourt(labels = NULL, show_zone_lines = FALSE, show_3m_line = !beach, label_font_size = font_size * 0.8, show_zones = FALSE, base_size = font_size, court = "upper", line_width = 0.3) +
            geom_text(data = ax %>% dplyr::filter(!.data$text_bw), aes(x = .data$ex, y = .data$ey, label = .data$text, angle = .data$la, hjust = .data$lhj), size = 1.5, colour = "white", lineheight = 1.0) +
            geom_text(data = ax %>% dplyr::filter(.data$text_bw), aes(x = .data$ex, y = .data$ey, label = .data$text, angle = .data$la, hjust = .data$lhj), size = 1.5, colour = "black", lineheight = 1.0) +
            scale_fill_gradientn(colors = cpal, labels = NULL, guide = "none") +
            theme(strip.background = ggplot2::element_rect(fill = NA, linewidth = 0, linetype = "blank"), strip.text.x = element_text(margin = ggplot2::margin(t = 0, b = 3))) +
            facet_wrap(~attack_side, ncol = 2)
        attr(p, "rel_size") <- 1.0
    } else if (attack_plot_style == "coordinates_lines") {
        ax <- ax %>% dplyr::filter(!is.na(.data$start_coordinate), !is.na(.data$end_coordinate))
        if (nrow(ax) < 1) return(NULL)
        ax <- flip_coords(ax)
        midx <- !is.na(ax$mid_coordinate_x)
        ax <- ax %>% mutate(evaluation = case_when(.data$evaluation == "Winning attack" ~ "Kill",
                                                   .data$evaluation %in% c("Blocked", "Error") ~ .data$evaluation,
                                                   TRUE ~ "In play"),
                            icon_event = case_when(.data$evaluation == "Kill" ~ "kill",
                                                   .data$evaluation == "Blocked" ~ "block",
                                                   .data$evaluation == "Error" ~ "error",
                                                   TRUE ~ "in_play"))
        if (is.data.frame(icons)) ax <- left_join(ax, icons %>% dplyr::select("icon_event", "icon_name", icon = "unicode"), by = "icon_event")
        p <- ggplot() + ggcourt(labels = NULL, show_3m_line = !beach, label_font_size = font_size * 0.8, show_zones = FALSE,
                                base_size = font_size, court = "full", ylim = c(2.5, NA_real_), line_width = 0.3)
        if (any(midx)) p <- p + geom_segment(data = ax[midx, ], aes(x = .data$start_coordinate_x, y = .data$start_coordinate_y, xend = .data$mid_coordinate_x, yend = .data$mid_coordinate_y), colour = "grey80", alpha = 0.5) +
                           geom_segment(data = ax[midx, ], aes(x = .data$mid_coordinate_x, y = .data$mid_coordinate_y, xend = .data$end_coordinate_x, yend = .data$end_coordinate_y), colour = "grey80", alpha = 0.5)
        p <- p + geom_segment(data = ax[!midx, ], aes(x = .data$start_coordinate_x, y = .data$start_coordinate_y, xend = .data$end_coordinate_x, yend = .data$end_coordinate_y), colour = "grey80", alpha = 0.5)
        if ("icon" %in% names(ax)) {
            p <- p + geom_text(data = ax, aes(x = .data$end_coordinate_x, y = .data$end_coordinate_y, label = .data$icon, colour = .data$evaluation), size = 2, family = "fa6s") +
                scale_colour_manual(values = c(Kill = "#008000", Error = "#800000", Blocked = "darkorange", `In play` = "black"), guide = "none")
        }
        attr(p, "rel_size") <- 1.05
        if ("icon_name" %in% names(ax)) attr(p, "icon_names") <- unique(na.omit(ax$icon_name)) ## find any icons used in the icon_name col, and pass those back as an attributexs
    } else if (attack_plot_style %in% c("subzones_heatmap", "coordinates_heatmap")) {
        if (attack_plot_style == "subzones_heatmap") {
            ax <- ax %>% dplyr::filter(!is.na(.data$end_zone), !is.na(.data$end_subzone), !.data$evaluation %in% c("Blocked", "Blocked for reattack"))
            if (nrow(ax) < 1) return(NULL)
            ## density by subzone, but line segments by zone to reduce the number of lines being plotted
            ## use net zone for segment start location, if we have it
            if ("attack_net_zone15" %in% names(ax)) {
                ax$sx <- (ax$attack_net_zone15 - 1) * 0.6 + 0.8
                ax$sy <- 3.3
            } else {
                ax <- cbind(ax, dv_xy(ax$start_zone, end = "lower", xynames = c("sx", "sy")))
            }
            ax <- cbind(ax, dv_xy(ax$end_zone, end = "upper", subzones = ax$end_subzone), dv_xy(ax$end_zone, end = "upper", xynames = c("ex", "ey")))
            segx <- ax %>% count(.data$sx, .data$sy, .data$ex, .data$ey)
        } else {
            ax <- ax %>% dplyr::filter(!is.na(.data$end_coordinate)) %>% flip_coords %>%
                dplyr::rename(x = "end_coordinate_x", y = "end_coordinate_y") %>% dplyr::filter(.data$y > 3.5)
            if (nrow(ax) < 1) return(NULL)
            segx <- ax %>% count(.data$start_coordinate_x, .data$start_coordinate_y, .data$mid_coordinate_x, .data$mid_coordinate_y, .data$ex, .data$ey)
        }
        if (beach) {
            ax <- mutate(ax, attack_side = case_when(.data$start_zone %in% c(4, 7, 5) ~ "From left",
                                                     .data$start_zone %in% c(3, 8, 6) ~ "From middle",
                                                     .data$start_zone %in% c(2, 9, 1) ~ "From right"),
                         attack_side = factor(.data$attack_side, levels = c("From left", "From middle", "From right")))
        } else {
            ## TODO change this to separate quick/slide/pipe from other
            ax <- mutate(ax, attack_side = case_when(.data$start_zone %in% c(4, 7, 5) ~ "From left",
                                                     .data$start_zone %in% c(3, 8, 6) ~ "From middle",
                                                     .data$start_zone %in% c(2, 9, 1) ~ "From right"),
                         attack_side = factor(.data$attack_side, levels = c("From left", "From middle", "From right")))
        }
        hx <- ovlytics::ov_heatmap_kde(x = ax %>% mutate(N = 1) %>% group_by(.data$attack_side), resolution = sub("_heatmap", "", attack_plot_style), court = "upper")
        p <- ggplot(hx, aes(.data$x, .data$y, fill = .data$density)) +
            scale_fill_gradientn(colors = cpalw, guide = "none") +
            geom_raster() + facet_wrap(~attack_side, ncol = 2)
        if (FALSE) {
            ## line segments
            if (grepl("coordinates", attack_plot_style)) {
                ## segments by coords
                midx <- !is.na(segx$mid_coordinate_x)
                if (any(midx)) p <- p + geom_segment(data = segx[midx, ], aes(x = .data$start_coordinate_x, y = .data$start_coordinate_y, xend = .data$mid_coordinate_x, yend = .data$mid_coordinate_y, linewidth = .data$n), colour = "grey80", alpha = 0.5, inherit.aes = FALSE) +
                                   geom_segment(data = segx[midx, ], aes(x = .data$mid_coordinate_x, y = .data$mid_coordinate_y, xend = .data$x, yend = .data$y, linewidth = .data$n), colour = "grey80", alpha = 0.5, inherit.aes = FALSE)
                p <- p + geom_segment(data = segx[!midx, ], aes(x = .data$start_coordinate_x, y = .data$start_coordinate_y, xend = .data$x, yend = .data$y, linewidth = .data$n), colour = "grey50", alpha = 0.5, inherit.aes = FALSE, arrow = grid::arrow(angle = 15, length = unit(1, "mm"), type = "closed"))
            } else {
                p <- p + geom_segment(data = segx, aes(x = .data$sx, y = .data$sy, xend = .data$ex, yend = .data$ey, linewidth = .data$n), colour = "grey50", alpha = 0.5, inherit.aes = FALSE, arrow = grid::arrow(angle = 15, length = unit(1, "mm"), type = "closed"))
            }
            p <- p + scale_linewidth(guide = "none", range = c(0.4, 0.8))
        }
        p <- p + ggcourt(labels = NULL, show_3m_line = !beach, label_font_size = font_size * 0.8, show_zones = FALSE, base_size = font_size, line_width = 0.3, court = "upper", xlim = if (grepl("coordinates", attack_plot_style)) c(0.25, 3.75) else c(0.5, 3.5), ylim = if (grepl("coordinates", attack_plot_style)) c(3.5, 6.75) else c(3.5, 6.5)) ## court = "full", ylim = c(2.5, 6.75)
        attr(p, "rel_size") <- 1.05
    } else {
        p <- NULL
    }
    if (!is.null(p)) p <- p + ggplot2::labs(caption = paste(strwrap(paste0(target_team, " attack"), 25, simplify = FALSE)[[1]], collapse = "\n")) + theme(plot.caption = element_text(hjust = 0.5))
    p
}

flip_coords <- function(z) {
    ## re-orient all attacks to start in lower half of court
    flipidx <- which(z$start_coordinate_y > 3.5)
    if (length(flipidx) > 0) {
        z$start_coordinate_x[flipidx] <- dv_flip_x(z$start_coordinate_x[flipidx])
        z$mid_coordinate_x[flipidx] <- dv_flip_x(z$mid_coordinate_x[flipidx])
        z$end_coordinate_x[flipidx] <- dv_flip_x(z$end_coordinate_x[flipidx])
        z$start_coordinate_y[flipidx] <- dv_flip_y(z$start_coordinate_y[flipidx])
        z$mid_coordinate_y[flipidx] <- dv_flip_y(z$mid_coordinate_y[flipidx])
        z$end_coordinate_y[flipidx] <- dv_flip_y(z$end_coordinate_y[flipidx])
    }
    z
}
