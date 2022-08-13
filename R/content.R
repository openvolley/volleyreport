vr_content_match_outcome <- function(vsx, kable_format) {
    vsx$meta$teams %>% dplyr::select(.data$team, .data$sets_won) %>%
        dplyr::rename(Teams = "team", 'Final score' = "sets_won") %>%
        kable(format = kable_format, escape = FALSE, col.names = c("MATCH RESULT", ""), table.attr = "class=\"widetable\"") %>% kable_styling(bootstrap_options = c("striped", "hover"), full_width = TRUE, font_size = vsx$base_font_size) %>%
        row_spec(1:2, bold = TRUE) %>%
        row_spec(0, bold = TRUE, color = vsx$css$header_colour, background = vsx$css$header_background ) %>%
        column_spec(1, border_left = vsx$css$border) %>%
        column_spec(2, border_right = vsx$css$border) %>%
        row_spec(2, extra_css = paste0("border-bottom:", vsx$css$border))
}

vr_content_match_date <- function(vsx, kable_format) {
    temp <- vsx$meta$match
    temp$time <- tryCatch(format(temp$date + temp$time, "%H:%M:%S"), error = function(e) temp$time)
    kable(temp %>% dplyr::select(.data$date, .data$time, .data$season, .data$league) %>% mutate_all(to_char_noNA) %>% pivot_longer(cols = 1:4) %>%
          mutate(name = str_to_title(.data$name)),
          format = kable_format, escape = FALSE, align = "l", col.names = NULL, table.attr = "class=\"widetable\"") %>%
        kable_styling(bootstrap_options = c("condensed"), full_width = TRUE, font_size = vsx$base_font_size * 9/12) %>%
        column_spec(1, bold = TRUE)
}

vr_content_match_refs <- function(vsx, kable_format) {
    kable(vsx$meta$more %>% dplyr::select(.data$referees, .data$city, .data$arena, .data$scout) %>% mutate_all(to_char_noNA) %>% pivot_longer(cols = 1:4) %>%
          mutate(name = str_to_title(.data$name)),
          format = kable_format, escape = FALSE, align = "l", col.names = NULL, table.attr = "class=\"widetable\"") %>%
        kable_styling(bootstrap_options = c("condensed"), full_width = TRUE, font_size = vsx$base_font_size * 9/12) %>%
        column_spec(1, bold = TRUE)
}

vr_content_partial_scores <- function(vsx, kable_format) {
    this <- vsx$meta$result
    have_partial <- "score_intermediate1" %in% names(this)
    if (have_partial) {
        this$score_intermediate1 <- gsub("[[:space:]]+", "", this$score_intermediate1)
        this$score_intermediate2 <- gsub("[[:space:]]+", "", this$score_intermediate2)
        this$score_intermediate3 <- gsub("[[:space:]]+", "", this$score_intermediate3)
        this <- this %>% mutate(Set = dplyr::row_number(),
                                "Partial score" = paste(.data$score_intermediate1, .data$score_intermediate2, .data$score_intermediate3, sep = " / "))
    } else {
        this <- this %>% mutate(Set = dplyr::row_number())
    }
    if (!"duration" %in% names(this)) this$duration <- NA_integer_
    duridx <- !is.na(this$duration)
    this$Set[duridx] <- paste0(this$Set[duridx], " (", this$duration[duridx], " mins)")
    out <- this %>% dplyr::select("Set", if (have_partial) "Partial score", Score = "score") %>%
        kable(format = kable_format, escape = FALSE, align = "r", table.attr = "class=\"widetable\"") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = vsx$base_font_size * 11/12) %>%
        row_spec(0, align = "l", bold = TRUE, color = vsx$css$header_colour, background = vsx$css$header_background)
    if (nrow(this) > 0) {
        out <- out %>% column_spec(1, width = "0.7in", border_left = vsx$css$border) %>%
            column_spec(2 + have_partial, border_right = vsx$css$border, bold = TRUE) %>%
            row_spec(nrow(this), extra_css = paste0("border-bottom:", vsx$css$border))
    }
    out
}


vr_content_team_summary <- function(vsx, kable_format, which_team = "home") {
    which_team <- match.arg(which_team, c("home", "visiting"))
    if (which_team == "home") {
        players <- vsx$meta$players_h
        teamfun <- datavolley::home_team
    } else {
        players <- vsx$meta$players_v
        teamfun <- datavolley::visiting_team
    }
    P_sum <- players %>% dplyr::select(.data$player_id, .data$number, .data$name, .data$starting_position_set1, .data$starting_position_set2, .data$starting_position_set3, .data$starting_position_set4, .data$starting_position_set5, .data$role) %>%
        add_row(player_id = "Team total", name = "Team total") %>%
        mutate(starting_position_set1 = case_when(!is.na(.data$starting_position_set1) & .data$role %eq% "libero" ~ "L", TRUE ~ stringr::str_replace(starting_position_set1, '\\*', '.')),
               starting_position_set2 = case_when(!is.na(.data$starting_position_set2) & .data$role %eq% "libero" ~ "L", TRUE ~ stringr::str_replace(starting_position_set2, '\\*', '.')),
               starting_position_set3 = case_when(!is.na(.data$starting_position_set3) & .data$role %eq% "libero" ~ "L", TRUE ~ stringr::str_replace(starting_position_set3, '\\*', '.')),
               starting_position_set4 = case_when(!is.na(.data$starting_position_set4) & .data$role %eq% "libero" ~ "L", TRUE ~ stringr::str_replace(starting_position_set4, '\\*', '.')),
               starting_position_set5 = case_when(!is.na(.data$starting_position_set5) & .data$role %eq% "libero" ~ "L", TRUE ~ stringr::str_replace(starting_position_set5, '\\*', '.'))) %>%
        dplyr::select(-"role") %>%
        left_join(volleyreport::vr_points(vsx$x, teamfun(vsx$x), vote = vsx$vote, style = vsx$style) %>% dplyr::select("player_id", if (vsx$vote) "vote", "Tot", if (vsx$style %in% "default") c("BP", "W-L")), by = "player_id") %>%
        left_join(volleyreport::vr_serve(vsx$x, teamfun(vsx$x), refx = vsx$refx, style = vsx$style), by = "player_id", suffix = c(".pts", ".ser")) %>%
        left_join(volleyreport::vr_reception(vsx$x, teamfun(vsx$x), refx = vsx$refx, file_type = vsx$file_type, style = vsx$style), by = "player_id", suffix = c(".ser", ".rec")) %>%
        left_join(volleyreport::vr_attack(vsx$x, teamfun(vsx$x), style = vsx$style), by = "player_id", suffix = c(".rec", ".att")) %>%
        left_join(volleyreport::vr_block(vsx$x, teamfun(vsx$x), style = vsx$style), by = "player_id")

    P_sum <- P_sum %>%
        mutate(starting_position_set1 = case_when(!is.na(.data$starting_position_set1) & .data$starting_position_set1 %in% c("1","2","3","4","5","6") ~ cell_spec(.data$starting_position_set1, kable_format, color = "white", align = "c", background = "#444444", bold = TRUE), 
                                                  !is.na(.data$starting_position_set1) & .data$starting_position_set1 %in% c(".", "L") ~ cell_spec(.data$starting_position_set1, kable_format, color = "white", align = "c", background = "#999999")),
               starting_position_set2 = case_when(!is.na(.data$starting_position_set2) & .data$starting_position_set2 %in% c("1","2","3","4","5","6") ~ cell_spec(.data$starting_position_set2, kable_format, color = "white", align = "c", background = "#444444", bold = TRUE), 
                                                  !is.na(.data$starting_position_set2) & .data$starting_position_set2 %in% c(".", "L") ~ cell_spec(.data$starting_position_set2, kable_format, color = "white", align = "c", background = "#999999")),
               starting_position_set3 = case_when(!is.na(.data$starting_position_set3) & .data$starting_position_set3 %in% c("1","2","3","4","5","6") ~ cell_spec(.data$starting_position_set3, kable_format, color = "white", align = "c", background = "#444444", bold = TRUE), 
                                                  !is.na(.data$starting_position_set3) & .data$starting_position_set3 %in% c(".", "L") ~ cell_spec(.data$starting_position_set3, kable_format, color = "white", align = "c", background = "#999999")),
               starting_position_set4 = case_when(!is.na(.data$starting_position_set4) & .data$starting_position_set4 %in% c("1","2","3","4","5","6") ~ cell_spec(.data$starting_position_set4, kable_format, color = "white", align = "c", background = "#444444", bold = TRUE), 
                                                  !is.na(.data$starting_position_set4) & .data$starting_position_set4 %in% c(".", "L") ~ cell_spec(.data$starting_position_set4, kable_format, color = "white", align = "c", background = "#999999")),
               starting_position_set5 = case_when(!is.na(.data$starting_position_set5) & .data$starting_position_set5 %in% c("1","2","3","4","5","6") ~ cell_spec(.data$starting_position_set5, kable_format, color = "white", align = "c", background = "#444444", bold = TRUE),
                                                  !is.na(.data$starting_position_set5) & .data$starting_position_set5 %in% c(".", "L") ~ cell_spec(.data$starting_position_set5, kable_format, color = "white", align = "c", background = "#999999")))


    P_sum <- P_sum %>% dplyr::select(-"player_id") %>% dplyr::arrange(.data$number) %>% na_if(0)

    ## note that in some cases the dvw file can contain starting positions for players in a set that doesn't have a result
    if (isTRUE(vsx$remove_nonplaying)) {
        if (!isTRUE(suppressWarnings(max(vsx$x$set_number, na.rm = TRUE) > 1))) {
            ## special case for first set, rely on starting position
            P_sum <- dplyr::filter(P_sum, !is.na(.data$starting_position_set1))
        } else {
            P_sum <- P_sum[rowSums(is.na(P_sum[, c(-1, -2)])) < (ncol(P_sum) - 2), ]
        }
    }
    notsets <- setdiff(1:5, seq_len(nrow(vsx$meta$result)))
    P_sum <- P_sum[, setdiff(colnames(P_sum), paste0("starting_position_set", notsets))]
    ## put 0s back in for W-L
    if ("W-L" %in% names(P_sum)) P_sum$`W-L`[is.na(P_sum$`W-L`)] <- 0L
    Rexc <- !isTRUE(grepl("perana", vsx$file_type)) ## perana have only R#+, not R#
    if (!Rexc && "(Exc%)" %in% names(P_sum)) P_sum <- dplyr::select(P_sum, -"(Exc%)")
    P_sum
}

vr_content_team_table <- function(vsx, kable_format, which_team = "home") {
    which_team <- match.arg(which_team, c("home", "visiting"))
    P_sum <- vr_content_team_summary(vsx = vsx, kable_format = kable_format, which_team = which_team)
    Rexc <- "(Exc%)" %in% names(P_sum) ## perana have only R#+, not R#
    expSO <- "expSO%" %in% names(P_sum)
    recEff <- "recEff%" %in% names(P_sum)
    expBP <- "expBP%" %in% names(P_sum)
    srvEff <- "srvEff%" %in% names(P_sum)
    if (which_team == "home") {
        teamfun <- datavolley::home_team
    } else {
        teamfun <- datavolley::visiting_team
    }
    kable(P_sum, format = "html", escape = FALSE, col.names = c("", "", seq_len(nrow(vsx$meta$result)), if (vsx$vote) "Vote", "Tot", if (vsx$style %in% c("default")) c("BP", "W-L"), "Tot", "Err", if (vsx$style %in% c("ov1")) "Ace" else "Pts", if (expBP) "expBP%", if (srvEff) "Eff%", "Tot", "Err", "Pos%", if (Rexc) "(Exc%)", if (expSO) "expSO%", if (recEff) "Eff%", "Tot", "Err", "Blo", if (vsx$style %in% c("ov1")) "Kill" else "Pts", if (vsx$style %in% c("ov1")) "K%" else "Pts%", "Pts"), table.attr = "class=\"widetable\"") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = vsx$base_font_size * 11/12) %>%
        column_spec(2, width = "1.8in") %>%
        add_header_above(c(setNames(2, teamfun(vsx$x)), "Set" = nrow(vsx$meta$result), "Points" = 1 + 2 * vsx$style %in% c("default") + vsx$vote, "Serve" = 3 + expBP + srvEff, "Reception" = 3 + Rexc + expSO + recEff, "Attack" = 5, "Blo" = 1), color = vsx$css$header_colour, background = vsx$css$header_background, bold = TRUE, line = FALSE, extra_css = "padding-bottom:2px;") %>% row_spec(0, bold = TRUE, color = vsx$css$header_colour, background = vsx$css$header_background, font_size = vsx$base_font_size * 10/12) %>%
        column_spec(1, border_left = vsx$css$border) %>% 
        column_spec(ncol(P_sum), border_right = vsx$css$border) %>%
        row_spec(which(P_sum$name == "Team total"), background = "lightgrey")
}


vr_content_team_staff <- function(vsx, kable_format, which_team = "home") {
    which_team <- match.arg(which_team, c("home", "visiting"))
    if (which_team == "home") {
        teamfun <- datavolley::home_team
    } else {
        teamfun <- datavolley::visiting_team
    }
    thisC <- vsx$meta$teams %>% dplyr::filter(.data$team %eq% teamfun(vsx$x)) %>% dplyr::select(.data$coach, .data$assistant) %>% pivot_longer(1:2) %>%
        mutate(name = str_to_title(.data$name))
    kable(thisC, format = "html", escape = FALSE, col.names = c("Staff", ""), table.attr = "class=\"widetable\"") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = vsx$base_font_size * 10/12) %>%
        row_spec(0, bold = TRUE, color = vsx$css$header_colour, background = vsx$css$header_background)  %>%
        column_spec(1, border_left = vsx$css$border) %>%
        column_spec(2, border_right = vsx$css$border) %>%
        row_spec(nrow(thisC), extra_css = paste0("border-bottom:", vsx$css$border))
}


vr_content_team_set_summary <- function(vsx, kable_format, which_team = "home") {
    which_team <- match.arg(which_team, c("home", "visiting"))
    if (which_team == "home") {
        teamfun <- datavolley::home_team
    } else {
        teamfun <- datavolley::visiting_team
    }
    thisSS <- vr_points(vsx$x, teamfun(vsx$x), by = "set", style = vsx$style) %>%
        left_join(volleyreport::vr_serve(vsx$x, teamfun(vsx$x), by = "set", refx = vsx$refx, style = vsx$style), by = "set_number", suffix = c(".pts", ".ser") ) %>%
        left_join(volleyreport::vr_reception(vsx$x, teamfun(vsx$x), by = "set", refx = vsx$refx, file_type = vsx$file_type, style = vsx$style), by = "set_number", suffix = c(".ser", ".rec") ) %>%
        left_join(volleyreport::vr_attack(vsx$x, teamfun(vsx$x), by = "set", style = vsx$style), by = "set_number", suffix = c(".rec", ".att") ) %>%
        left_join(volleyreport::vr_block(vsx$x, teamfun(vsx$x), by = "set", style = vsx$style), by = "set_number") %>%
        mutate(set_number = paste("Set", .data$set_number)) %>% na_if(0)
    Rexc <- !isTRUE(grepl("perana", vsx$file_type)) && "(Exc%)" %in% names(thisSS) ## perana have only R#+, not R#
    if (!Rexc && "(Exc%)" %in% names(thisSS)) thisSS <- dplyr::select(thisSS, -"(Exc%)")
    expSO <- "expSO%" %in% names(thisSS)
    recEff <- "recEff%" %in% names(thisSS)
    expBP <- "expBP%" %in% names(thisSS)
    srvEff <- "srvEff%" %in% names(thisSS)
    kable(thisSS,format = "html", escape = TRUE, col.names = c("","Ser", "Atk", "Blo", "Op.Err", "Tot", "Err", if (vsx$style %in% c("ov1")) "Ace" else "Pts", if (expBP) "expBP%", if (srvEff) "Eff%", "Tot", "Err", "Pos%", if (Rexc) "(Exc%)", if (expSO) "expSO%", if (recEff) "Eff%", "Tot", "Err", "Blo", if (vsx$style %in% c("ov1")) "Kill" else "Pts", if (vsx$style %in% c("ov1")) "K%" else "Pts%", "Pts"), table.attr = "class=\"widetable\"") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = vsx$base_font_size * 11/12) %>%
        add_header_above(c("Set" = 1, "Points" = 4, "Serve" = 3 + expBP + srvEff, "Reception" = 3 + Rexc + expSO + recEff, "Attack" = 5, "Blo" = 1), color = vsx$css$header_colour, background = vsx$css$header_background, line = FALSE, extra_css = "padding-bottom:2px;") %>%
        row_spec(0, bold = TRUE, color = vsx$css$header_colour, background = vsx$css$header_background, font_size = vsx$base_font_size * 10/12) %>%
        column_spec(1, border_left = vsx$css$border) %>%
        column_spec(ncol(thisSS), border_right = vsx$css$border) %>%
        row_spec(nrow(thisSS), extra_css = paste0("border-bottom:", vsx$css$border))
}

vr_content_points_by_rot <- function(vsx, kable_format, which_team = "home") {
    which_team <- match.arg(which_team, c("home", "visiting"))
    out <- vsx$x %>% dplyr::filter(.data$skill == "Serve")
    if (which_team == "home") {
        out <- group_by(out, .data$home_setter_position) %>%
            dplyr::summarize(Diff = sum(.data$point_won_by == .data$home_team, na.rm = TRUE) - sum(.data$point_won_by == .data$visiting_team, na.rm = TRUE)) %>%
            dplyr::arrange(dplyr::desc(.data$home_setter_position))
        if (vsx$style %in% c("ov1")) {
            out <- left_join(out, vsx$x %>% dplyr::filter(.data$skill == "Serve" & .data$team == .data$home_team) %>% group_by(.data$home_setter_position) %>%
                                  dplyr::summarize(`N srv` = n(), `BP%` = prc(round(mean0(.data$point_won_by == .data$team) * 100)),
                                                   `srvEff%` = prc(round(serve_eff(.data$evaluation) * 100)),
                                                   `expBP%` = prc(round(mean0(.data$expBP) * 100))),
                             by = "home_setter_position")
            out <- left_join(out, vsx$x %>% dplyr::filter(.data$skill == "Reception" & .data$team == .data$home_team) %>% group_by(.data$home_setter_position) %>%
                                  dplyr::summarize(`N rec` = n(), `SO%` = prc(round(mean0(.data$point_won_by == .data$team) * 100)),
                                                   `recEff%` = prc(round(reception_eff(.data$evaluation) * 100)),
                                                   `expSO%` = prc(round(mean0(.data$expSO) * 100))),
                             by = "home_setter_position")
        }
        out <- dplyr::rename(out, `S in` = .data$home_setter_position)
    } else {
        out <- group_by(out, .data$visiting_setter_position) %>%
            dplyr::summarize(Diff = sum(.data$point_won_by == .data$visiting_team, na.rm = TRUE) - sum(.data$point_won_by == .data$home_team, na.rm = TRUE)) %>%
            dplyr::arrange(dplyr::desc(.data$visiting_setter_position))
        if (vsx$style %in% c("ov1")) {
            out <- left_join(out, vsx$x %>% dplyr::filter(.data$skill == "Serve" & .data$team == .data$visiting_team) %>% group_by(.data$visiting_setter_position) %>%
                                  dplyr::summarize(`N srv` = n(), `BP%` = prc(round(mean0(.data$point_won_by == .data$team) * 100)),
                                                   `srvEff%` = prc(round(serve_eff(.data$evaluation) * 100)),
                                                   `expBP%` = prc(round(mean0(.data$expBP) * 100))),
                             by = "visiting_setter_position")
            out <- left_join(out, vsx$x %>% dplyr::filter(.data$skill == "Reception" & .data$team == .data$visiting_team) %>% group_by(.data$visiting_setter_position) %>%
                                  dplyr::summarize(`N rec` = n(), `SO%` = prc(round(mean0(.data$point_won_by == .data$team) * 100)),
                                                   `recEff%` = prc(round(reception_eff(.data$evaluation) * 100)),
                                                   `expSO%` = prc(round(mean0(.data$expSO) * 100))),
                             by = "visiting_setter_position")
        }
        out <- dplyr::rename(out, `S in` = .data$visiting_setter_position)
    }
    out$`S in` <- factor(out$`S in`, levels = c(1, 6:2))
    out <- dplyr::arrange(out, .data$`S in`)
    if (nrow(out) > 0) {
        if (vsx$style %in% c("ov1")) {
            out <- if (is.null(vsx$refx)) dplyr::select(out, -"expBP%", -"expSO%") else dplyr::select(out, -"srvEff%", -"recEff%")
            out <- as.data.frame(t(dplyr::rename(out, "Pts diff" = "Diff")))
            colnames(out) <- paste0("P", out[1, ])
            out <- out[-1, ]
            kable(out, format = kable_format, escape = FALSE, rownames = TRUE, align = "r", table.attr = "class=\"widetable\"") %>%
                kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = vsx$base_font_size * 9/12) %>%
                row_spec(0, bold = TRUE, color = vsx$css$header_colour, background = vsx$css$header_background, font_size = vsx$base_font_size * 10/12)
        } else {
            kable(na.omit(out), format = kable_format, escape = FALSE, align = "r", table.attr = "class=\"widetable\"") %>%
                kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = vsx$base_font_size * 10/12) %>%
                row_spec(0, bold = TRUE, color = vsx$css$header_colour, background = vsx$css$header_background, font_size = vsx$base_font_size * 10/12) %>%
                add_header_above(c("Points" = 2), color = vsx$css$header_colour, background = vsx$css$header_background, line = FALSE, extra_css = "padding-bottom:2px;")
        }
    }
}


vr_content_team_each <- function(vsx, kable_format, which_team = "home") {
    which_team <- match.arg(which_team, c("home", "visiting"))
    if (which_team == "home") {
        teamfun <- datavolley::home_team
        otherteamfun <- datavolley::visiting_team
    } else {
        teamfun <- datavolley::visiting_team
        otherteamfun <- datavolley::home_team
    }
    rthis <- vsx$x %>%
        dplyr::summarize(Receptions = sum(.data$skill == "Reception" & .data$team == teamfun(vsx$x), na.rm = TRUE),
                         'Earned pts SO' = sum(.data$serving_team == otherteamfun(vsx$x) & .data$skill %in% c("Attack", "Block") & grepl("^Winning ", .data$evaluation) & .data$team == teamfun(vsx$x), na.rm = TRUE)) %>%
        pivot_longer(cols = 1:2)

    sthis <- vsx$x %>% dplyr::filter(.data$team == teamfun(vsx$x)) %>%
        dplyr::summarize(Serves = sum(.data$skill == "Serve", na.rm = TRUE),
                         'Earned pts BP' = sum(.data$serving_team == teamfun(vsx$x)  & .data$skill %in% c("Serve", "Attack", "Block") & grepl("^(Ace|Winning) ", .data$evaluation), na.rm = TRUE)) %>%
        pivot_longer(cols = 1:2)

    list(kable(rthis, format = kable_format, escape = FALSE, align = "l", col.names = NULL, table.attr = "class=\"widetable\"") %>% kable_styling(bootstrap_options = c("condensed"), font_size = vsx$base_font_size * 10/12),
         kable(paste0("Each ", round(rthis$value[1]/rthis$value[2], 2), " receptions 1 point"), format = kable_format, escape = FALSE, align = "l", col.names = NULL, table.attr = "class=\"widetable\"") %>% kable_styling(bootstrap_options = c("condensed"), font_size = vsx$base_font_size * 10/12),
         kable(sthis, format = kable_format, escape = FALSE, align = "l", col.names = NULL, table.attr = "class=\"widetable\"") %>% kable_styling(bootstrap_options = c("condensed"), font_size = vsx$base_font_size * 10/12),
         kable(paste0("Each ", round(sthis$value[1]/sthis$value[2], 2), " serves 1 breakpoint"), format = kable_format, escape = FALSE, align = "l", col.names = NULL, table.attr = "class=\"widetable\"") %>% kable_styling(bootstrap_options = c("condensed"), font_size = vsx$base_font_size * 10/12)
         )
}

vr_content_key <- function(vsx, kable_format) {
    data.frame(Label = c("BP", "Err", "Pos%", if (vsx$style %in% c("default")) "W-L", if (vsx$style %in% c("ov1")) "K%" else "Pts", "Blo", if (vsx$style %in% c("default")) "Exc", if (vsx$style %in% c("ov1")) { if (!is.null(vsx$refx) && nrow(vsx$refx) > 0) c("expSO%", "expBP%") else c("srvEff%", "recEff%") }, if (vsx$style %in% c("ov1")) "P*n*" else "Earned pts", ".", "*n*"),
               Description = c("Break point", "Errors", "Positive +#", if (vsx$style %in% c("default")) "Won-Lost", if (vsx$style %in% c("ov1")) "Attack kill%" else "Points", "Blocked", if (vsx$style %in% c("default")) "Excellent", if (vsx$style %in% c("ov1")) { if (!is.null(vsx$refx) && nrow(vsx$refx) > 0) c("Expected SO%", "Expected BP%") else c("Serve efficiency", "Reception efficiency") }, if (vsx$style %in% c("ov1")) "Setter in *n*" else "Aces, attack and block kills", "Substitute", "Starting position")) %>%
        kable(format = kable_format, align = c("r", "l"), escape = FALSE, col.names = NULL, table.attr = "class=\"widetable\"") %>%
        kable_styling(font_size = vsx$base_font_size * 9/12) %>%
        ## add outer framing to make the key visually separate from the content
        column_spec(1, border_left = vsx$css$border) %>% column_spec(2, border_right = vsx$css$border) %>%
        row_spec(1, extra_css = paste0("border-top:", vsx$css$border)) %>%
        row_spec(10, extra_css = paste0("border-bottom:", vsx$css$border))
}

vr_content_kill_on_rec <- function(vsx, kable_format, eval_codes = c("#", "+", "#+"), hdr = "1st Attack AFTER POSITIVE RECEPTION (+#)") {
    KoRhome <- vsx$x %>% dplyr::filter(.data$skill == "Attack" & .data$ts_pass_evaluation_code %in% eval_codes & .data$phase == "Reception" & .data$team == datavolley::home_team(vsx$x)) %>%
        dplyr::summarize(Err = sum(.data$evaluation_code == "="), Blo = sum(.data$evaluation_code == "/"),
                         'Pts%' = prc(round(mean0(.data$evaluation_code == "#") * 100)), Tot = n())
    if (vsx$style %in% c("ov1")) KoRhome <- dplyr::rename(KoRhome, "K%" = "Pts%")

    KoRvis <- vsx$x %>% dplyr::filter(.data$skill == "Attack" & .data$ts_pass_evaluation_code %in% eval_codes & .data$phase == "Reception" & .data$team == datavolley::visiting_team(vsx$x)) %>%
        dplyr::summarize(Err = sum(.data$evaluation_code == "="), Blo = sum(.data$evaluation_code == "/"),
                         'Pts%' = prc(round(mean0(.data$evaluation_code == "#") * 100)), Tot = n())
    if (vsx$style %in% c("ov1")) KoRvis <- dplyr::rename(KoRvis, "K%" = "Pts%")

    hd <- c(8)
    names(hd) <- hdr
    kable(cbind(KoRhome, KoRvis[4:1]), format = kable_format, escape = FALSE, align = "c", table.attr = "class=\"widetable\"") %>% kable_styling(bootstrap_options = c("condensed"), font_size = vsx$base_font_size * 10/12) %>%
        column_spec(4, border_right = vsx$css$border) %>%
        row_spec(0, color = vsx$css$header_colour, background = vsx$css$header_background) %>%
        add_header_above(hd, color = vsx$css$header_colour, background = vsx$css$header_background, line = FALSE, extra_css = "padding-bottom:2px;")
}


vr_content_kill_in_trans <- function(vsx, kable_format) {
    KiThome <- vsx$x %>% dplyr::filter(.data$skill == "Attack" & .data$phase == "Transition" & .data$team == datavolley::home_team(vsx$x)) %>%
        dplyr::summarize(Err = sum(.data$evaluation_code == "="),
                         Blo = sum(.data$evaluation_code == "/"),
                         'Pts%' = round(mean0(.data$evaluation_code == "#"), 2) * 100, Tot = n())
    if (vsx$style %in% c("ov1")) KiThome <- dplyr::rename(KiThome, "K%" = "Pts%")

    KiTvis <- vsx$x %>% dplyr::filter(.data$skill == "Attack" & .data$phase == "Transition" & .data$team == datavolley::visiting_team(vsx$x)) %>%
        dplyr::summarize(Err = sum(.data$evaluation_code == "="),
                         Blo = sum(.data$evaluation_code == "/"),
                         'Pts%' = round(mean0(.data$evaluation_code == "#"), 2) * 100, Tot = n())
    if (vsx$style %in% c("ov1")) KiTvis <- dplyr::rename(KiTvis, "K%" = "Pts%")

    kable(cbind(KiThome, KiTvis[4:1]),format = vsx$format, escape = FALSE, align = "c", table.attr = "class=\"widetable\"") %>%
        kable_styling(bootstrap_options = c("condensed"), font_size = vsx$base_font_size * 10/12) %>%
        column_spec(4, border_right = vsx$css$border) %>%
        row_spec(0, color = vsx$css$header_colour, background = vsx$css$header_background) %>%
        add_header_above(c("Attack on dig" = 8), color = vsx$css$header_colour, background = vsx$css$header_background, line = FALSE, extra_css = "padding-bottom:2px;")
}
