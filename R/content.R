vr_content_match_outcome <- function(vsx, kable_format) {
    vsx$meta$teams %>% dplyr::select("team", "sets_won") %>%
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
    ## do we need to enforce max nchars in league, season?
    kable(temp %>% dplyr::select("date", "time", "season", "league") %>% mutate_all(to_char_noNA) %>% pivot_longer(cols = 1:4) %>%
          mutate(name = str_to_title(.data$name)),
          format = kable_format, escape = FALSE, align = "l", col.names = NULL, table.attr = "class=\"widetable\"") %>%
        kable_styling(bootstrap_options = c("condensed"), full_width = TRUE, font_size = vsx$base_font_size * 9/12) %>%
        column_spec(1, bold = TRUE)
}

vr_content_match_refs <- function(vsx, kable_format) {
    this <- tibble(referees = if ("referees" %in% names(vsx$meta$more)) vsx$meta$more$referees else "", city = if ("city" %in% names(vsx$meta$more)) vsx$meta$more$city else "", arena = if ("arena" %in% names(vsx$meta$more)) vsx$meta$more$arena else "", scout = if ("scout" %in% names(vsx$meta$more)) vsx$meta$more$scout else "")
    kable(this %>% mutate_all(to_char_noNA) %>% pivot_longer(cols = 1:4) %>% mutate(name = str_to_title(.data$name)),
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
    fontscale <- 11/12
    if (vsx$style %in% c("ov1")) {
        this$duration <- as.character(this$duration)
        if (any(duridx)) this$duration[tail(which(duridx), 1)] <- paste0(this$duration[tail(which(duridx), 1)], " mins")
        out <- bind_rows(tidyr::pivot_wider(this[, c("Set", "score"), drop = FALSE], names_from = "Set", values_from = "score"),
                         tidyr::pivot_wider(this[, c("Set", "duration"), drop = FALSE], names_from = "Set", values_from = "duration"))
        if (ncol(out) > 0) {
            names(out)[1] <- paste0("Set ", names(out)[1])
            #out <- as.data.frame(out)
            #rownames(out) <- c("Score", "Duration")
            out <- kable(out, format = kable_format, escape = FALSE, align = "r", table.attr = "class=\"widetable\"", row.names = FALSE) %>%
                kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = vsx$base_font_size * fontscale) %>%
                row_spec(0, align = "r", bold = TRUE, color = vsx$css$header_colour, background = vsx$css$header_background)
            if (nrow(this) > 0) {
                out <- out %>% column_spec(1, ##width = "0.7in",
                                           border_left = vsx$css$border) %>%
                    column_spec(nrow(this), border_right = vsx$css$border) %>%
                    row_spec(1, bold = TRUE) %>%
                    row_spec(2, extra_css = paste0("border-bottom:", vsx$css$border), font_size = 0.9 * vsx$base_font_size * fontscale)
            }
        } else {
            out <- NULL
        }
    } else {
        this$Set[duridx] <- paste0(this$Set[duridx], " (", this$duration[duridx], " mins)")
        out <- this %>% dplyr::select("Set", if (have_partial) "Partial score", Score = "score") %>%
            kable(format = kable_format, escape = FALSE, align = "r", table.attr = "class=\"widetable\"") %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = vsx$base_font_size * fontscale) %>%
            row_spec(0, align = "l", bold = TRUE, color = vsx$css$header_colour, background = vsx$css$header_background)
        if (nrow(this) > 0) {
            out <- out %>% column_spec(1, width = "0.7in", border_left = vsx$css$border) %>%
                column_spec(2 + have_partial, border_right = vsx$css$border, bold = TRUE) %>%
                row_spec(nrow(this), extra_css = paste0("border-bottom:", vsx$css$border))
        }
    }
    out
}


update_lineups <- function(x) {
    is_beach <- any(grepl("beach", x$meta$match$regulation))
    pseq <- seq_len(if (is_beach) 2L else 6L)
    if (is.null(x$plays) || nrow(x$plays) < 1) return(x)
    ## starting lineups and subs
    ## this can be done even for sets that haven't been completed
    for (si in seq_len(max(x$plays$set_number, na.rm = TRUE))) {
        ## use the final >LUp row for starting lineup
        final_lup_row <- which(x$plays$set_number == si & grepl(">LUp", x$plays$code, ignore.case = TRUE))
        if (length(final_lup_row) > 0) final_lup_row <- max(final_lup_row)
        if (length(final_lup_row) == 1) {
            home_starting_lineup <- as.numeric(x$plays[final_lup_row, paste0("home_p", pseq)])
            if (!paste0("starting_position_set", si) %in% names(x$meta$players_h)) x$meta$players_h[[paste0("starting_position_set", si)]] <- NA_character_
            for (j in seq_along(home_starting_lineup)) {
                pl_row <- which(x$meta$players_h$number == home_starting_lineup[j])
                if (length(pl_row) == 1) x$meta$players_h[[paste0("starting_position_set", si)]][pl_row] <- as.character(j)
            }
            ## subs
            all_home_pl <- unique(na.omit(as.numeric(unlist(x$plays[which(x$plays$set_number == si & !grepl(">LUp", x$plays$code, ignore.case = TRUE)), paste0("home_p", pseq)]))))
            home_subs <- na.omit(setdiff(all_home_pl, home_starting_lineup))
            x$meta$players_h[[paste0("starting_position_set", si)]][x$meta$players_h$number %in% home_subs] <- "*"
            ## visiting team
            visiting_starting_lineup <- as.numeric(x$plays[final_lup_row, paste0("visiting_p", pseq)])
            if (!paste0("starting_position_set", si) %in% names(x$meta$players_v)) x$meta$players_v[[paste0("starting_position_set", si)]] <- NA_character_
            for (j in seq_along(visiting_starting_lineup)) {
                pl_row <- which(x$meta$players_v$number == visiting_starting_lineup[j])
                if (length(pl_row) == 1) x$meta$players_v[[paste0("starting_position_set", si)]][pl_row] <- as.character(j)
            }
            ## subs
            all_visiting_pl <- unique(na.omit(as.numeric(unlist(x$plays[which(x$plays$set_number == si & !grepl(">LUp", x$plays$code, ignore.case = TRUE)), paste0("visiting_p", pseq)]))))
            visiting_subs <- na.omit(setdiff(all_visiting_pl, visiting_starting_lineup))
            x$meta$players_v[[paste0("starting_position_set", si)]][x$meta$players_v$number %in% visiting_subs] <- "*"
        }
    }
    x
}

vr_content_team_summary <- function(vsx, kable_format, which_team = "home") {
    which_team <- match.arg(which_team, c("home", "visiting"))

    nsets <- max(vsx$x$set_number, na.rm = TRUE)
    if (!grepl("beach", vsx$file_type) && nsets > 5) {
        ## with 5 + golden (6 set) matches, the $meta component only holds the starting/sub info for the first 5 sets
        temp <- update_lineups(list(meta = vsx$meta, plays = vsx$x)) ## gives lineup/subs for 6th set
        thismeta <- vsx$meta
        thismeta$players_h$starting_position_set6 <- temp$meta$players_h$starting_position_set6
        thismeta$players_v$starting_position_set6 <- temp$meta$players_v$starting_position_set6
    } else {
        thismeta <- vsx$meta
    }

    if (which_team == "home") {
        players <- dplyr::as_tibble(thismeta$players_h)
        teamfun <- datavolley::home_team
        tchar <- "\\*"
    } else {
        players <- dplyr::as_tibble(thismeta$players_v)
        teamfun <- datavolley::visiting_team
        tchar <- "a"
    }
    if (!"starting_position_set1" %in% names(players)) players$starting_position_set1 <- NA_character_
    if (!"starting_position_set2" %in% names(players)) players$starting_position_set2 <- NA_character_
    if (!"starting_position_set3" %in% names(players)) players$starting_position_set3 <- NA_character_
    if (!"starting_position_set4" %in% names(players)) players$starting_position_set4 <- NA_character_
    if (!"starting_position_set5" %in% names(players)) players$starting_position_set5 <- NA_character_
    if (!"starting_position_set6" %in% names(players)) players$starting_position_set6 <- NA_character_
    ## starting setters, per set
    ## avoid >LUp (lineup) rows, can get carryover from preceding set into these lines. Lineups should be settled after the initial >LUps per set
    if (!grepl("beach", vsx$file_type)) {
        if (which_team == "home") {
            ss <- vsx$x %>% dplyr::filter(!grepl(">LUp|\\*\\*.set", .data$code, ignore.case = TRUE), !is.na(.data$set_number), !is.na(.data$home_setter_position), !is.na(.data$home_player_id1), !is.na(.data$home_player_id2), !is.na(.data$home_player_id3), !is.na(.data$home_player_id4), !is.na(.data$home_player_id5), !is.na(.data$home_player_id6)) %>%
                group_by(.data$set_number) %>% slice(1L) %>%
                dplyr::summarize(setter_id = case_when(.data$home_setter_position == 1 ~ .data$home_player_id1,
                                                       .data$home_setter_position == 2 ~ .data$home_player_id2,
                                                       .data$home_setter_position == 3 ~ .data$home_player_id3,
                                                       .data$home_setter_position == 4 ~ .data$home_player_id4,
                                                       .data$home_setter_position == 5 ~ .data$home_player_id5,
                                                       .data$home_setter_position == 6 ~ .data$home_player_id6)) %>% ungroup
        } else {
            ss <- vsx$x %>% dplyr::filter(!grepl(">LUp|\\*\\*.set", .data$code, ignore.case = TRUE), !is.na(.data$set_number), !is.na(.data$visiting_setter_position), !is.na(.data$visiting_player_id1), !is.na(.data$visiting_player_id2), !is.na(.data$visiting_player_id3), !is.na(.data$visiting_player_id4), !is.na(.data$visiting_player_id5), !is.na(.data$visiting_player_id6)) %>%
                group_by(.data$set_number) %>% slice(1L) %>%
                dplyr::summarize(setter_id = case_when(.data$visiting_setter_position == 1 ~ .data$visiting_player_id1,
                                                       .data$visiting_setter_position == 2 ~ .data$visiting_player_id2,
                                                       .data$visiting_setter_position == 3 ~ .data$visiting_player_id3,
                                                       .data$visiting_setter_position == 4 ~ .data$visiting_player_id4,
                                                       .data$visiting_setter_position == 5 ~ .data$visiting_player_id5,
                                                       .data$visiting_setter_position == 6 ~ .data$visiting_player_id6)) %>% ungroup
        }
        for (si in seq_len(min(nsets, nrow(ss)))) {
            idx <- players$player_id %eq% ss$setter_id[si]
            if (sum(idx) == 1) {
                players[idx, paste0("starting_position_set", si)] <- paste0(players[idx, paste0("starting_position_set", si)], "S")
            }
        }
    }
    P_sum <- players %>% dplyr::select("player_id", "number", "name", "starting_position_set1", "starting_position_set2", "starting_position_set3", "starting_position_set4", "starting_position_set5", "starting_position_set6", "role") %>%
        add_row(player_id = "Team total", name = "Team total") %>%
        mutate(starting_position_set1 = case_when(!is.na(.data$starting_position_set1) & .data$role %eq% "libero" ~ "L",
                                                  TRUE ~ stringr::str_replace(starting_position_set1, '\\*', '.')),
               starting_position_set2 = case_when(!is.na(.data$starting_position_set2) & .data$role %eq% "libero" ~ "L",
                                                  TRUE ~ stringr::str_replace(starting_position_set2, '\\*', '.')),
               starting_position_set3 = case_when(!is.na(.data$starting_position_set3) & .data$role %eq% "libero" ~ "L",
                                                  TRUE ~ stringr::str_replace(starting_position_set3, '\\*', '.')),
               starting_position_set4 = case_when(!is.na(.data$starting_position_set4) & .data$role %eq% "libero" ~ "L",
                                                  TRUE ~ stringr::str_replace(starting_position_set4, '\\*', '.')),
               starting_position_set5 = case_when(!is.na(.data$starting_position_set5) & .data$role %eq% "libero" ~ "L",
                                                  TRUE ~ stringr::str_replace(starting_position_set5, '\\*', '.')),
               starting_position_set6 = case_when(!is.na(.data$starting_position_set6) & .data$role %eq% "libero" ~ "L",
                                                  TRUE ~ stringr::str_replace(starting_position_set6, '\\*', '.'))) %>%
        dplyr::select(-"role") %>%
        left_join(volleyreport::vr_points(vsx$x, teamfun(vsx$x), vote = vsx$vote, style = vsx$style) %>% dplyr::select("player_id", if (vsx$vote) "vote", "Tot", if (vsx$style %in% "default") c("BP", "W-L")), by = "player_id") %>%
        left_join(volleyreport::vr_serve(vsx$x, teamfun(vsx$x), refx = vsx$refx, style = vsx$style), by = "player_id", suffix = c(".pts", ".ser")) %>%
        left_join(volleyreport::vr_reception(vsx$x, teamfun(vsx$x), refx = vsx$refx, file_type = vsx$file_type, style = vsx$style), by = "player_id", suffix = c(".ser", ".rec")) %>%
        left_join(volleyreport::vr_attack(vsx$x, teamfun(vsx$x), file_type = vsx$file_type, style = vsx$style), by = "player_id", suffix = c(".rec", ".att")) %>%
        left_join(volleyreport::vr_block(vsx$x, teamfun(vsx$x), style = vsx$style), by = "player_id")
    if (grepl("beach", vsx$file_type)) {
        ## for beach, add blocker/defender and side if possible
        rs <- beach_guess_roles_sides(vsx$x, which_team = which_team)
        if (nrow(rs) == 2 && !any(is.na(rs$player_beach_role)) && !any(is.na(rs$player_beach_side))) {
            P_sum <- left_join(P_sum, rs, by = "player_id") %>%
                mutate(name = case_when(.data$name == "Team total" ~ .data$name,
                                        TRUE ~ paste0(.data$name, " (", .data$player_beach_side, "/", .data$player_beach_role, ")"))) %>%
                dplyr::select(-"player_beach_role", -"player_beach_side")
        }
    } else if (vsx$style %in% c("ov1")) {
        ## indoor, can we figure out which played subbed for which?
        for (st in seq_len(nsets)) {
            try({
                sub_codes <- vsx$x$code[which(vsx$x$set_number == st & grepl(paste0("^", tchar, "[Cc][[:digit:]\\:\\.]+$"), vsx$x$code))]
                sbs <- bind_rows(lapply(sub_codes, function(cd) {
                    list(p_out = as.numeric(sub("^.[Cc]", "", sub("[\\:\\.][[:digit:]]+$", "", cd))), p_in = as.numeric(sub("^.[Cc][[:digit:]]+[\\:\\.]", "", cd)))
                }))
                if (nrow(sbs) > 0) {
                    sbs <- distinct(sbs)
                    subs_in <- P_sum %>% dplyr::filter(.data[[paste0("starting_position_set", st)]] == ".")
                    sbs <- sbs %>% group_by(.data$p_in) %>% dplyr::summarize(p_out = paste0("!", paste(unique(.data$p_out), collapse = ","))) %>% ungroup
                    for (i in seq_len(nrow(sbs))) {
                        pidx <- P_sum$number %eq% sbs$p_in[i] & P_sum[[paste0("starting_position_set", st)]] %eq% "."
                        P_sum[pidx, paste0("starting_position_set", st)] <- sbs$p_out[i]
                    }
                }
            })
        }
    }

    ## cell spec for libero/sub/setter cells
    lsspec <- function(z, border = FALSE) cell_spec(z, kable_format, color = "white", align = "c", background = "#999999", extra_css = if (border) "border:1px solid black;" else NULL)
    sspec <- function(z) cell_spec(sub("S", "", z), kable_format, color = "black", align = "c", background = "#FFF", bold = TRUE, extra_css = "border:1px solid black;")
    P_sum <- P_sum %>%
        mutate(starting_position_set1 = case_when(.data$starting_position_set1 %in% c(1:6) ~ cell_spec(.data$starting_position_set1, kable_format, color = "white", align = "c", background = "#444444", bold = TRUE),
                                                  .data$starting_position_set1 %in% paste0(c(1:6), "S") ~ sspec(.data$starting_position_set1),
                                                  .data$starting_position_set1 %in% c(".", "L") ~ lsspec(.data$starting_position_set1),
                                                  !is.na(.data$starting_position_set1) & grepl("^!", .data$starting_position_set1) ~ lsspec(sub("!", "", .data$starting_position_set1), border = TRUE)),
               starting_position_set2 = case_when(.data$starting_position_set2 %in% c(1:6) ~ cell_spec(.data$starting_position_set2, kable_format, color = "white", align = "c", background = "#444444", bold = TRUE),
                                                  .data$starting_position_set2 %in% paste0(c(1:6), "S") ~ sspec(.data$starting_position_set2),
                                                  .data$starting_position_set2 %in% c(".", "L") ~ lsspec(.data$starting_position_set2),
                                                  !is.na(.data$starting_position_set2) & grepl("^!", .data$starting_position_set2) ~ lsspec(sub("!", "", .data$starting_position_set2), border = TRUE)),
               starting_position_set3 = case_when(.data$starting_position_set3 %in% c(1:6) ~ cell_spec(.data$starting_position_set3, kable_format, color = "white", align = "c", background = "#444444", bold = TRUE),
                                                  .data$starting_position_set3 %in% paste0(c(1:6), "S") ~ sspec(.data$starting_position_set3),
                                                  .data$starting_position_set3 %in% c(".", "L") ~ lsspec(.data$starting_position_set3),
                                                  !is.na(.data$starting_position_set3) & grepl("^!", .data$starting_position_set3) ~ lsspec(sub("!", "", .data$starting_position_set3), border = TRUE)),
               starting_position_set4 = case_when(.data$starting_position_set4 %in% c(1:6) ~ cell_spec(.data$starting_position_set4, kable_format, color = "white", align = "c", background = "#444444", bold = TRUE),
                                                  .data$starting_position_set4 %in% paste0(c(1:6), "S") ~ sspec(.data$starting_position_set4),
                                                  .data$starting_position_set4 %in% c(".", "L") ~ lsspec(.data$starting_position_set4),
                                                  !is.na(.data$starting_position_set4) & grepl("^!", .data$starting_position_set4) ~ lsspec(sub("!", "", .data$starting_position_set4), border = TRUE)),
               starting_position_set5 = case_when(.data$starting_position_set5 %in% c(1:6) ~ cell_spec(.data$starting_position_set5, kable_format, color = "white", align = "c", background = "#444444", bold = TRUE),
                                                  .data$starting_position_set5 %in% paste0(c(1:6), "S") ~ sspec(.data$starting_position_set5),
                                                  .data$starting_position_set5 %in% c(".", "L") ~ lsspec(.data$starting_position_set5),
                                                  !is.na(.data$starting_position_set5) & grepl("^!", .data$starting_position_set5) ~ lsspec(sub("!", "", .data$starting_position_set5), border = TRUE)),
               starting_position_set6 = case_when(.data$starting_position_set6 %in% c(1:6) ~ cell_spec(.data$starting_position_set6, kable_format, color = "white", align = "c", background = "#444444", bold = TRUE),
                                                  .data$starting_position_set6 %in% paste0(c(1:6), "S") ~ sspec(.data$starting_position_set6),
                                                  .data$starting_position_set6 %in% c(".", "L") ~ lsspec(.data$starting_position_set6),
                                                  !is.na(.data$starting_position_set6) & grepl("^!", .data$starting_position_set6) ~ lsspec(sub("!", "", .data$starting_position_set6), border = TRUE)))

    P_sum <- P_sum %>% dplyr::select(-"player_id") %>% dplyr::arrange(.data$number) %>% mutate(across(where(is.numeric), ~na_if(., 0)))

    ## note that in some cases the dvw file can contain starting positions for players in a set that doesn't have a result
    if (!grepl("beach", vsx$file_type) && isTRUE(vsx$remove_nonplaying)) {
        if (!isTRUE(suppressWarnings(max(vsx$x$set_number, na.rm = TRUE) > 1))) {
            ## special case for first set, rely on starting position
            P_sum <- dplyr::filter(P_sum, !is.na(.data$starting_position_set1))
        } else {
            P_sum <- P_sum[rowSums(is.na(P_sum[, c(-1, -2)])) < (ncol(P_sum) - 2), ]
        }
    }
    notsets <- setdiff(1:6, seq_len(nrow(thismeta$result)))
    P_sum <- P_sum[, setdiff(colnames(P_sum), paste0("starting_position_set", notsets))]
    ## put 0s back in for W-L
    if ("W-L" %in% names(P_sum)) P_sum$`W-L`[is.na(P_sum$`W-L`)] <- 0L
    Rexc <- !isTRUE(grepl("perana", vsx$file_type)) ## perana have only R#+, not R#
    if (!Rexc && "(Exc%)" %in% names(P_sum)) P_sum <- dplyr::select(P_sum, -"(Exc%)")
    if (grepl("beach", vsx$file_type)) {
        P_sum <- P_sum[, !grepl("^starting_position_set", names(P_sum))]
        P_sum$number <- NA_integer_
    }
    P_sum
}


first_serve <- function(x, file_type) {
    temp <- x %>% dplyr::filter(.data$skill == "Serve") %>% group_by(.data$set_number) %>% slice(1L) %>% ungroup %>% dplyr::summarize(srv = case_when(.data$team == .data$home_team ~ "H", .data$team == .data$visiting_team ~ "V", TRUE ~ "U")) %>% dplyr::pull(.data$srv)
    ## in some cases we might not have the first serve of a set, either because the scout missed it or it was e.g. a rotation error and not scouted
    ## for beach we can't do much about this but for indoor we can correct one such error in the first 4 sets (not set 5 or 6/golden set)
    if (!grepl("beach", file_type)) {
        smax <- min(4, length(temp)) ## which sets are we looking at here
        opts <- substr(c("HVHV", "VHVH"), 1, smax)
        actual <- paste(temp[seq_len(smax)], collapse = "")
        tempd <- adist(actual, opts)[1, ] ## levenshtein distance
        if (min(tempd) == 1) { ## one disagreement between actual values and possible options, we can fix this
            closest <- which.min(tempd)
            temp[seq_len(smax)] <- strsplit(opts[closest], "")[[1]]
        }
    }
    temp[temp %eq% "H"] <- "home"
    temp[temp %eq% "V"] <- "visiting"
    temp[temp %eq% "U"] <- "unknown"
    temp
}

vr_content_team_table <- function(vsx, kable_format, which_team = "home") {
    which_team <- match.arg(which_team, c("home", "visiting"))
    P_sum <- vr_content_team_summary(vsx = vsx, kable_format = kable_format, which_team = which_team)
    Rexc <- "(Exc%)" %in% names(P_sum) ## perana have only R#+, not R#
    expSO <- "expSO%" %in% names(P_sum)
    SO <- "SO%" %in% names(P_sum)
    modSO <- "modSO%" %in% names(P_sum)
    recEff <- "recEff%" %in% names(P_sum)
    expBP <- "expBP%" %in% names(P_sum)
    BP <- "BP%" %in% names(P_sum)
    srvEff <- "srvEff%" %in% names(P_sum)
    attEff <- "attEff%" %in% names(P_sum)
    if (which_team == "home") {
        teamfun <- datavolley::home_team
    } else {
        teamfun <- datavolley::visiting_team
    }
    on2 <- "On2" %in% names(P_sum)
    spcols <- grep("^starting_position_set", names(P_sum)) ## won't be present for beach
    bcols <- if (vsx$style %in% c("ov1")) 2 + length(spcols) + vsx$vote + cumsum(c(1, 3 + expBP + srvEff + BP, 3 + Rexc + expSO + recEff + modSO + SO, 5 + attEff + 2 * on2)) else NULL ## internal right-borders

    if (length(spcols) < 1) {
        set_col_hdr <- character()
    } else {
        set_col_hdr <- seq_len(min(6, length(spcols)))
        ## indicate first-serving team in each set
        try({
            fss <- first_serve(vsx$x, file_type = vsx$file_type)
            for (k in seq_along(set_col_hdr)) {
                if (fss[k] %eq% which_team) {
                    ## team served first, so use circled number with font size and layout adjustment
                    set_col_hdr[k] <- paste0("<span style='font-size:125%; vertical-align:top;'>", circled1to6[k], "</span>")
                    ## or could just underline the set number
                    ##set_col_hdr[k] <- paste0("<span style='text-decoration:underline;'>", set_col_hdr[k], "</span>")
                } else {
                    set_col_hdr[k] <- paste0("<span style='vertical-align:-5%;'>", set_col_hdr[k], "</span>")
                }
            }
        })
    }

    ## col names
    cn <- c("", "", set_col_hdr, if (vsx$vote) "Vote", "Tot", if (vsx$style %in% c("default")) c("BP", "W-L"), "Tot", "Err", if (vsx$style %in% c("ov1")) "Ace" else "Pts", if (expBP) "expBP%", if (srvEff) "Eff%", if (BP) "BP%", "Tot", "Err", "Pos%", if (Rexc) "(Exc%)", if (expSO) "expSO%", if (recEff) "Eff%", if (modSO) "modSO%", if (SO) "SO%", "Tot", "Err", "Blo", if (vsx$style %in% c("ov1")) "Kill" else "Pts", if (vsx$style %in% c("ov1")) "K%" else "Pts%", if (attEff) "Eff%", if (on2) c("(On2)", "(On2 K%)"), "Pts")
    ## header above col names
    ch <- c(setNames(2, teamfun(vsx$x)), if (length(spcols) > 0) setNames(min(6, length(spcols)), "Set"), "Points" = 1 + 2 * vsx$style %in% c("default") + vsx$vote, "Serve" = 3 + expBP + srvEff + BP, "Reception" = 3 + Rexc + expSO + recEff + modSO + SO, "Attack" = 5 + 2 * on2 + attEff, "Blo" = 1)
    calign <- c(rep("l", 2), rep("r", ncol(P_sum) - 2)) ## right-align everything after the player names and starting position columns
    col_widths <- list(number = if (grepl("beach", vsx$file_type)) 0 else 4, name = if (!grepl("beach", vsx$file_type)) 40 else 36,
                       sets = 8, points = 12) ## TODO put these outside of the function so that they are visible from the set summary func
    cwu <- "mm"
    out <- kable(P_sum, format = "html", escape = FALSE, col.names = cn, table.attr = "class=\"widetable\"", align = calign) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = vsx$base_font_size * 10/12) %>%
        add_header_above(ch, color = vsx$css$header_colour, background = vsx$css$header_background, bold = TRUE, line = FALSE, extra_css = "padding-bottom:2px;") %>% row_spec(0, bold = TRUE, color = vsx$css$header_colour, background = vsx$css$header_background, font_size = vsx$base_font_size * 10/12) %>%
        column_spec(1, width = paste0(col_widths$number, cwu), border_left = vsx$css$border) %>%
        column_spec(2, width = paste0(col_widths$name, cwu))
    if (length(spcols) > 0) out <- out %>% column_spec(2 + seq_len(min(6, length(spcols))), width = paste0(col_widths$sets, cwu))
    out <- out %>% column_spec(2 + (if (length(spcols) > 0) min(6, length(spcols)) else 0) + 1, width = paste0(col_widths$points, cwu)) %>%
        column_spec(ncol(P_sum), border_right = vsx$css$border) %>%
        row_spec(which(P_sum$name == "Team total"), background = "lightgrey") %>%
        row_spec(nrow(P_sum), extra_css = paste0("border-bottom:", vsx$css$border, "; padding-bottom:2px;"))
    if (length(bcols) > 0) out <- out %>% column_spec(bcols, border_right = "1px solid #CCC")
    out
}

## used to indicate first team serving in a set
circled1to6 <- strsplit(intToUtf8(9312:9317), "")[[1]]

vr_content_team_staff <- function(vsx, kable_format, which_team = "home") {
    which_team <- match.arg(which_team, c("home", "visiting"))
    if (which_team == "home") {
        teamfun <- datavolley::home_team
    } else {
        teamfun <- datavolley::visiting_team
    }
    thisC <- vsx$meta$teams %>% dplyr::filter(.data$team %eq% teamfun(vsx$x)) %>% dplyr::select("coach", "assistant") %>% pivot_longer(1:2) %>%
        mutate(name = str_to_title(.data$name))
    ## if no staff details, return NULL
    if (nrow(thisC) < 1 || all(is.na(thisC$value) | !nzchar(thisC$value))) return(NULL)
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
        left_join(volleyreport::vr_attack(vsx$x, teamfun(vsx$x), by = "set", file_type = vsx$file_type, style = vsx$style), by = "set_number", suffix = c(".rec", ".att") ) %>%
        left_join(volleyreport::vr_block(vsx$x, teamfun(vsx$x), by = "set", style = vsx$style), by = "set_number") %>%
        mutate(set_number = paste("Set", .data$set_number)) %>% mutate(across(where(is.numeric), ~na_if(., 0)))
    Rexc <- !isTRUE(grepl("perana", vsx$file_type)) && "(Exc%)" %in% names(thisSS) ## perana have only R#+, not R#
    if (!Rexc && "(Exc%)" %in% names(thisSS)) thisSS <- dplyr::select(thisSS, -"(Exc%)")
    expSO <- "expSO%" %in% names(thisSS)
    recEff <- "recEff%" %in% names(thisSS)
    expBP <- "expBP%" %in% names(thisSS)
    BP <- "BP%" %in% names(thisSS)
    srvEff <- "srvEff%" %in% names(thisSS)
    BP <- "BP%" %in% names(thisSS)
    modSO <- "modSO%" %in% names(thisSS)
    SO <- "SO%" %in% names(thisSS)
    attEff <- "attEff%" %in% names(thisSS)
    ## for beach, we aren't showing the set columns with starting positions so indicate first-serving team in each set in this table instead
    if (grepl("beach", vsx$file_type)) {
        try({
            fss <- first_serve(vsx$x, file_type = vsx$file_type)
            for (k in seq_len(nrow(thisSS))) {
                if (fss[k] %eq% which_team) {
                    ## team served first, so use circled number with font size and layout adjustment
                    thisSS$set_number[k] <- paste0("Set <span style='font-size:125%; vertical-align:middle;'>", circled1to6[k], "</span>")
                ##} else {
                ##    set_col_hdr[k] <- paste0("<span style='vertical-align:-5%;'>", set_col_hdr[k], "</span>")
                }
            }
        })
    }
    on2 <- "On2" %in% names(thisSS)
    bcols <- if (vsx$style %in% c("ov1")) 1 + cumsum(c(0, 4, 3 + BP + expBP + srvEff, 3 + Rexc + modSO + expSO + recEff + SO, 5 + 2 * on2 + attEff)) else NULL ## internal right-borders
    calign <- c("l", rep("r", ncol(thisSS) - 1)) ## right-align everything after the set number
    out <- kable(thisSS,format = "html", escape = FALSE, col.names = c("","Ser", "Atk", "Blo", "Op.Err", "Tot", "Err", if (vsx$style %in% c("ov1")) "Ace" else "Pts", if (expBP) "expBP%", if (srvEff) "Eff%", if (BP) "BP%", "Tot", "Err", "Pos%", if (Rexc) "(Exc%)", if (expSO) "expSO%", if (recEff) "Eff%", if (modSO) "modSO%", if (SO) "SO%", "Tot", "Err", "Blo", if (vsx$style %in% c("ov1")) "Kill" else "Pts", if (vsx$style %in% c("ov1")) "K%" else "Pts%", if (attEff) "Eff%", if (on2) c("(On2)", "(On2 K%)"), "Pts"), table.attr = "class=\"widetable\"", align = calign) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = vsx$base_font_size * 10/12) %>%
        add_header_above(c("Set" = 1, "Points" = 4, "Serve" = 3 + BP + expBP + srvEff, "Reception" = 3 + modSO + Rexc + expSO + recEff + SO, "Attack" = 5 + 2 * on2 + attEff, "Blo" = 1), color = vsx$css$header_colour, background = vsx$css$header_background, line = FALSE, extra_css = "padding-bottom:2px;") %>%
        row_spec(0, bold = TRUE, color = vsx$css$header_colour, background = vsx$css$header_background, font_size = vsx$base_font_size * 10/12) %>%
        column_spec(1, border_left = vsx$css$border) %>%
        column_spec(ncol(thisSS), border_right = vsx$css$border)
    if (length(bcols) > 0) out <- out %>% column_spec(bcols, border_right = "1px solid #CCC")
    row_spec(out, nrow(thisSS), extra_css = paste0("border-bottom:", vsx$css$border))
}

vr_content_points_by_rot <- function(vsx, kable_format, which_team = "home") {
    ## NB this is almost certainly meaningless for beach
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
                                  dplyr::summarize(`N rec` = n(), `modSO%` = prc(round(mean0(.data$point_won_by == .data$team) * 100)),
                                                   `recEff%` = prc(round(reception_eff(.data$evaluation) * 100)),
                                                   `expSO%` = prc(round(mean0(.data$expSO) * 100))),
                             by = "home_setter_position")
        }
        out <- dplyr::rename(out, `S in` = "home_setter_position")
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
                                  dplyr::summarize(`N rec` = n(), `modSO%` = prc(round(mean0(.data$point_won_by == .data$team) * 100)),
                                                   `recEff%` = prc(round(reception_eff(.data$evaluation) * 100)),
                                                   `expSO%` = prc(round(mean0(.data$expSO) * 100))),
                             by = "visiting_setter_position")
        }
        out <- dplyr::rename(out, `S in` = "visiting_setter_position")
    }
    out$`S in` <- factor(out$`S in`, levels = c(1, 6:2))
    out <- dplyr::arrange(out, .data$`S in`)
    if (nrow(out) > 0) {
        if (vsx$style %in% c("ov1")) {
            out <- if (is.null(vsx$refx)) dplyr::select(out, -"expBP%", -"expSO%") else dplyr::select(out, -"srvEff%", -"recEff%")
            out <- as.data.frame(t(dplyr::rename(out, "Pts diff" = "Diff")))
            colnames(out) <- paste0("P", out[1, ])
            out <- out[-1, , drop = FALSE]
            kable(out, format = kable_format, escape = FALSE, row.names = TRUE, align = "r", table.attr = "class=\"widetable\"") %>%
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

    miss2 <- function(z) ifelse(is.na(z) | is.infinite(z), "?", z) ## replace missing or infinites with "?"
    list(kable(rthis, format = kable_format, escape = FALSE, align = "l", col.names = NULL, table.attr = "class=\"widetable\"") %>% kable_styling(bootstrap_options = c("condensed"), font_size = vsx$base_font_size * 10/12),
         kable(paste0("Each ", miss2(round(rthis$value[1]/rthis$value[2], 2)), " receptions 1 point"), format = kable_format, escape = FALSE, align = "l", col.names = NULL, table.attr = "class=\"widetable\"") %>% kable_styling(bootstrap_options = c("condensed"), font_size = vsx$base_font_size * 10/12),
         kable(sthis, format = kable_format, escape = FALSE, align = "l", col.names = NULL, table.attr = "class=\"widetable\"") %>% kable_styling(bootstrap_options = c("condensed"), font_size = vsx$base_font_size * 10/12),
         kable(paste0("Each ", miss2(round(sthis$value[1]/sthis$value[2], 2)), " serves 1 breakpoint"), format = kable_format, escape = FALSE, align = "l", col.names = NULL, table.attr = "class=\"widetable\"") %>% kable_styling(bootstrap_options = c("condensed"), font_size = vsx$base_font_size * 10/12)
         )
}

vr_content_key <- function(vsx, kable_format, rows, cols = 2, icon_names = character()) {
    if (missing(cols)) cols <- if (missing(rows)) 2L else NA_integer_
    if (missing(rows)) rows <- NA_integer_
    beach <- grepl("beach", vsx$file_type)
    receff_txt <- paste0("Reception efficiency<br /><span style=\"font-size:", vsx$base_font_size * 5/12, "pt\">(Perf + Pos - Err - Overpasses) / N</span>")
    srveff_txt <- paste0("Serve efficiency<br /><span style=\"font-size:", vsx$base_font_size * 5/12, "pt\">(Ace + Pos - Err - Poor) / N</span>")
    this_icons <- if (is.data.frame(vsx$plot_icons)) vsx$plot_icons else vr_plot_icons()
    out <- tribble(
        ~Label, ~Description,
        if (beach) "L/R/Blk/Def" else "", "Played left/right, as blocker/defender",
        "BP", "Break point",
        "Err", "Errors",
        "Pos%", "Positive +#",
        if (vsx$style %in% c("default")) "W-L" else "", if (vsx$style %in% c("default")) "Won-Lost" else "",
        if (vsx$style %in% c("ov1")) "K%" else "Pts", if (vsx$style %in% c("ov1")) "Attack kill%" else "Points",
        "Blo", "Blocked",
        if (vsx$style %in% c("default")) "Exc" else "", if (vsx$style %in% c("default")) "Excellent" else "",
        if (vsx$style %in% c("ov1")) { if (!is.null(vsx$refx)) "expSO%" else "Srv eff%" } else "", if (vsx$style %in% c("ov1")) { if (!is.null(vsx$refx)) "Expected SO%" else srveff_txt } else "",
        if (vsx$style %in% c("ov1")) { if (!is.null(vsx$refx)) "expBP%" else "Rec eff%" } else "", if (vsx$style %in% c("ov1")) { if (!is.null(vsx$refx)) "Expected BP%" else receff_txt } else "",
        if (vsx$style %in% c("ov1") && !beach) "P*x*" else "", "Setter in *x*",
        if (!vsx$style %in% c("ov1")) "Earned pts" else "", "Aces, attack and block kills",
        ##if (vsx$style %in% c("ov1")) "P*x*" else "Earned pts", if (vsx$style %in% c("ov1")) "Setter in *x*" else "Aces, attack and block kills",
        if (vsx$style %in% c("ov1")) "modSO%" else "", "SO% on non-error serves",
        if (!beach) "*n*" else "", "Starting position",
        if (!beach) "*n*" else "", "Starting setter",
        if (beach) paste0("Set <span style='font-size:125%; vertical-align:middle;'>", circled1to6[1], "</span>") else "", "Served first in set",
        if (!beach) "." else "", if (vsx$style %in% c("ov1")) "Substituted for player p" else "Substitute")
    icon_names <- unique(na.omit(icon_names))
    if (length(icon_names) > 0) {
        out <- bind_rows(out, lapply(sort(icon_names), function(ic) list(Label = get_plot_icon(ic, this_icons, as = "svg"), Description = get_plot_icon(ic, this_icons, as = "description"))))
    }
    out <- out[nzchar(out$Label), ]
    nidx <- which(out$Label == "*n*")
    if (length(nidx) > 0) out$Label[nidx[1]] <- cell_spec("n", kable_format, color = "white", align = "c", background = "#444444", bold = TRUE)
    if (length(nidx) > 1) out$Label[nidx[2]] <- cell_spec("n", kable_format, color = "black", align = "c", background = "#FFF", bold = TRUE, extra_css = "border:1px solid black;")
    if (vsx$style %in% c("ov1")) {
        out$Label[out$Label == "."] <- cell_spec("p", kable_format, color = "white", align = "c", background = "#999999", extra_css = "border:1px solid black;")
    }
    if (!is.na(rows)) {
        cols <- ceiling(nrow(out) / rows) * 2
    } else {
        ## cols is a multiple of two: each pair of columns contains a label and its description
        if (cols %% 2 != 0) stop("cols should be a multiple of 2")
        cols <- max(2, cols)
    }
    if (cols > 2) {
        rows_per_col <- ceiling(2 * nrow(out) / cols)
        out <- bind_rows(out, tibble(Label = NA_character_, Description = NA_character_)[rep(1, rows_per_col * cols / 2 - nrow(out)), ]) ## add blank rows to pad the bottom
        out <- do.call(cbind, lapply(seq(1, nrow(out), by = rows_per_col), function(i) out[i:min(nrow(out), (i + rows_per_col - 1L)), ]))
    }
    out %>% kable(format = kable_format, align = c("r", "l"), escape = FALSE, col.names = NULL, table.attr = "class=\"widetable\"") %>%
        kable_styling(font_size = vsx$base_font_size * 6.5/12) %>%
        ## add outer framing to make the key visually separate from the content
        column_spec(seq(1, ncol(out), by = 2), border_left = vsx$css$border) %>%
        column_spec(ncol(out), border_right = vsx$css$border) %>%
        row_spec(1, extra_css = paste0("padding-top:2px; border-top:", vsx$css$border)) %>%
        row_spec(nrow(out), extra_css = paste0("padding-bottom:2px; border-bottom:", vsx$css$border))
}

## kill on reception or in transition
##  eval_codes ignored for transition
vr_content_kill_rec_trans <- function(vsx, which_team = "both", rec_trans = "rec", kable_format, eval_codes = c("#", "+", "#+"), hdr = "1st attack after pos. reception (R+#)", hide_col_names = FALSE) {
    which_team <- match.arg(which_team, c("home", "visiting", "both"))
    rec_trans <- match.arg(rec_trans, c("rec", "trans")) ## on reception or in transition?
    if (which_team %in% c("home", "both")) {
        temp <- if (rec_trans == "rec") {
                    vsx$x %>% dplyr::filter(.data$skill == "Attack" & .data$ts_pass_evaluation_code %in% eval_codes & .data$phase == "Reception" & .data$team == datavolley::home_team(vsx$x))
                } else {
                    vsx$x %>% dplyr::filter(.data$skill == "Attack" & .data$phase == "Transition" & .data$team == datavolley::home_team(vsx$x))
                }
        Khome <- temp %>% dplyr::summarize(Err = sum(.data$evaluation_code == "="),
                                           Blo = sum(.data$evaluation_code == "/"),
                                           'Pts%' = prc(round(mean0(.data$evaluation_code == "#") * 100)),
                                           Tot = n())
        if (vsx$style %in% c("ov1")) Khome <- dplyr::rename(Khome, "K%" = "Pts%")
    }

    if (which_team %in% c("visiting", "both")) {
        temp <- if (rec_trans == "rec") {
                    vsx$x %>% dplyr::filter(.data$skill == "Attack" & .data$ts_pass_evaluation_code %in% eval_codes & .data$phase == "Reception" & .data$team == datavolley::visiting_team(vsx$x))
                } else {
                    vsx$x %>% dplyr::filter(.data$skill == "Attack" & .data$phase == "Transition" & .data$team == datavolley::visiting_team(vsx$x))
                }
        Kvis <- temp %>% dplyr::summarize(Err = sum(.data$evaluation_code == "="),
                                          Blo = sum(.data$evaluation_code == "/"),
                                          'Pts%' = prc(round(mean0(.data$evaluation_code == "#") * 100)),
                                          Tot = n())
        if (vsx$style %in% c("ov1")) Kvis <- dplyr::rename(Kvis, "K%" = "Pts%")
    }

    hd <- if (!is.null(hdr)) setNames(which_team %in% c("home", "both") * 4 + which_team %in% c("visiting", "both") * 4, hdr) else NULL
    tbl_content <- if (which_team == "home") Khome else if (which_team == "visiting") Kvis else cbind(Khome, Kvis[4:1])
    out <- kable(tbl_content, format = kable_format, escape = FALSE, align = "c", col.names = if (hide_col_names) NULL else names(tbl_content), table.attr = "class=\"widetable\"") %>% kable_styling(bootstrap_options = c("condensed"), font_size = vsx$base_font_size * 10/12) %>%
        column_spec(4, border_right = vsx$css$border)
    if (!hide_col_names) out <- out %>% row_spec(0, color = vsx$css$header_colour, background = vsx$css$header_background, font_size = vsx$base_font_size * 9/12)
    if (!is.null(hdr)) out <- out %>% add_header_above(hd, color = vsx$css$header_colour, background = vsx$css$header_background, line = FALSE, extra_css = paste0("padding-bottom:2px;", if (hide_col_names) paste0("border-bottom:", vsx$css$border)), font_size = vsx$base_font_size * 9/12)
    if (which_team != "both") {
        out %>% column_spec(1, border_left = vsx$css$border) %>% row_spec(1, extra_css = paste0("border-bottom:", vsx$css$border))
    } else {
        out
    }
}
