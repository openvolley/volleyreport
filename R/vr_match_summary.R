#' Generate single-page match summary report
#'
#' @param x datavolley or string: as returned by \code{datavolley::dv_read}, or the path to such a file
#' @param outfile string: path to file to produce (if not specified, will create a file in the temporary directory)
#' @param refx data.frame: some choices of `style` require a reference data set to calculate e.g. expected SO. This should be from comparison matches (e.g. all matches from the same league), and should be a data.frame of the plays components from those matches. If missing, expected SO and BP will be replaced by reception and serve efficiency
#' @param vote logical: include vote report component? If not explicitly specified, `vote` might be set to FALSE depending on `style`
#' @param format string: "pdf" (using latex-based PDF), "paged_pdf" (using pagedown-based PDF), "png", "paged_png", or "html"
#' @param icon string: (optional) filename of icon image to use
#' @param css list: css specifications for some elements, giving (currently fairly limited) control over appearance. See the output of \code{\link{vr_css}} for an example. Note that some styling does not seem to be applied when exporting to PDF
#' @param remove_nonplaying logical: if \code{TRUE}, remove players from the team summaries that did not take to the court
#' @param style string: can be
#' * "default" - the standard FIVB match report
#' * "ov1" - modified version of "default" with score evolution plot, different breakdown by rotation, and other changes
#' @param skill_evaluation_decode : as for [datavolley::dv_read()]
#' @param shiny_progress logical: if \code{TRUE}, the report generation process will issue \code{shiny::setProgress()} calls. The call to \code{vr_match_summary} should therefore be wrapped in a \code{shiny::withProgress()} scope
#' @param chrome_print_extra_args character: additional parameters to pass as `extra_args` to [pagedown::chrome_print()] (only relevant if using a "paged_*" format)
#' @param ... : additional parameters passed to the rmarkdown template
#' @return The path to the report file
#'
#' @examples
#' \donttest{
#'   f <- vr_match_summary(dv_example_file(), format = "paged_pdf")
#'   if (interactive()) browseURL(f)
#' }
#' @export
vr_match_summary <- function(x, outfile, refx, vote = TRUE, format = "html", icon = NULL, css = vr_css(), remove_nonplaying = TRUE, style = "default", skill_evaluation_decode = "guess", shiny_progress = FALSE, chrome_print_extra_args = NULL, ...) {
    if (is.string(x) && file.exists(x)) {
        if (grepl("\\.dvw$", x, ignore.case = TRUE)) {
            x <- datavolley::dv_read(x, skill_evaluation_decode = skill_evaluation_decode)
        } else {
            stop("unknown file format: ", x)
        }
    }
    assert_that(inherits(x, c("datavolley", "peranavolley")))
    assert_that(is.string(format))
    assert_that(is.flag(shiny_progress), !is.na(shiny_progress))
    format <- tolower(format)
    format <- match.arg(format, c("html", "pdf", "png", "paged_pdf", "paged_png"))
    if (format %in% c("pdf", "png")) {
        ## check that we have phantomjs installed
        if (!webshot::is_phantomjs_installed()) {
            stop("phantomjs must be installed for pdf/png format. See help('install_phantomjs', 'webshot')")
        }
    }
    style <- check_report_style(style)
    dots <- list(...)
    if ("footnotes" %in% names(dots)) {
        footnotes <- dots$footnotes
        dots$footnotes <- NULL
    } else {
        footnotes <- c()
    }
    if (style %in% c("ov1")) {
        if (missing(vote)) vote <- FALSE
        if (!missing(refx)) {
            if (identical(refx, "self")) {
                ## undocumented for now
                refx <- datavolley::plays(x)
                footnotes <- c(footnotes, "Expected SO/BP use this match as reference data.")
            } else if (!is.data.frame(refx) || nrow(refx) < 1) {
                refx <- NULL
            }
        } else {
            refx <- NULL
        }
    } else {
        ## refx not used
        refx <- NULL
    }
    if (!missing(css) && !is.null(css)) {
        css0 <- vr_css()
        for (nm in names(css)) css0[[nm]] <- css[[nm]]
        css <- css0
    }
    team <- datavolley::home_team(x)
    meta <- x$meta
    final_format <- sub("paged_", "", format)
    if (!grepl("paged_", format)) format <- "html" ## even for pdf, treat now as html then webshot to pdf from that
    working_dir <- tempfile()
    dir.create(working_dir)
    rmd_template <- file.path(working_dir, paste0(if (grepl("paged", format)) "paged_pdf" else format, ".Rmd"))
    if (!file.copy(from = system.file(file.path("extdata", paste0(if (grepl("paged", format)) "paged_pdf" else format, ".Rmd")), package = "volleyreport"), to = rmd_template))
        stop("cannot copy template file to temporary directory")

    if (final_format %in% c("pdf", "png")) {
        final_outfile <- if (missing(outfile)) tempfile(fileext = paste0(".", final_format)) else outfile
        outfile <- tempfile(fileext = ".html")
    } else {
        if (missing(outfile)) outfile <- tempfile(fileext = paste0(".", format))
    }
    file_type <- NULL ## indoor (datavolley) or perana_indoor or potentially beach
    if ("file_meta" %in% names(x)) file_type <- x$file_meta$file_type
    x <- datavolley::plays(x)
    if (is.null(file_type)) {
        ## figure out what data type we have
        file_type <- guess_data_type(x)
    }
    if (shiny_progress) try(shiny::setProgress(value = 0.1, message = "Preprocessing data"), silent = TRUE)
    ## some data fixes
    for (col in c("start_zone", "end_zone", "end_subzone", "start_coordinate", "start_coordinate_x", "start_coordinate_y", "end_coordinate", "end_coordinate_x", "end_coordinate_y")) { ## also do for coords
        idx <- which(x$skill %eq% "Reception" & is.na(x[[col]]) & lag(x$skill) %eq% "Serve")
        if (length(which) > 0) x[[col]][idx] <- lag(x[[col]])[idx]
        idx <- which(x$skill %eq% "Serve" & is.na(x[[col]]) & lead(x$skill) %eq% "Reception")
        if (length(which) > 0) x[[col]][idx] <- lead(x[[col]])[idx]
    }
    ## add some extra cols
    if (!"end_cone" %in% names(x)) x$end_cone <- NA_integer_
    if (!"receiving_team" %in% names(x)) {
        x <- mutate(x, receiving_team = case_when(.data$serving_team %eq% .data$home_team ~ .data$visiting_team,
                                                  .data$serving_team %eq% .data$visiting_team ~ .data$home_team))
    }
    if (!"breakpoint/sideout" %in% names(x)) {
        x <- mutate(x, `breakpoint/sideout` = case_when(.data$team %eq% .data$receiving_team ~ "Sideout",
                                                        .data$team %eq% .data$serving_team ~ "Breakpoint"))
    }
    if (!"setter_position" %in% names(x)) {
        x <- mutate(x, setter_position = case_when(.data$team %eq% .data$home_team ~ .data$home_setter_position,
                                                   .data$team %eq% .data$visiting_team ~ .data$visiting_setter_position))
    }
    if (!"opposing_team" %in% names(x)) {
        x <- mutate(x, opposing_team = case_when(.data$team %eq% .data$home_team ~ .data$visiting_team,
                                                 .data$team %eq% .data$visiting_team ~ .data$home_team))
    }
    if (!"freeball_over" %in% names(x)) {
        ## "Freeball" skill can be used both for sending a freeball to the opposition as well as receiving one, so disambiguate these usages
        x <- mutate(x, freeball_over = .data$skill %eq% "Freeball",
                    lag(.data$match_id) %eq% .data$match_id, ##lead(.data$match_id) %eq% .data$match_id,
                    lag(.data$point_id) %eq% .data$point_id, ##lead(.data$point_id) %eq% .data$point_id,
                    ((!is.na(lead(.data$team)) & lead(.data$team) != .data$team) | lag(.data$team) %eq% .data$team))
    }

    starting_nrow <- nrow(x)
    if (!"ts_pass_evaluation_code" %in% names(x)) {
        touchsum <- x %>% ungroup %>% dplyr::filter(!is.na(.data$team)) %>% group_by_at(c("match_id", "team", "team_touch_id")) %>%
            dplyr::summarize(ts_pass_evaluation_code = single_value_or_na_char(na.omit(.data$evaluation_code[.data$skill %in% c("Reception", "Dig") | (.data$skill %eq% "Freeball" & !.data$freeball_over)]))) %>%
            ungroup %>% dplyr::select(-"team")
        x <- left_join(x, touchsum, by = c("match_id", "team_touch_id"))
    }

    if (!is.null(refx)) {
        lso <- refx %>% dplyr::filter(.data$skill == "Reception") %>% group_by(.data$evaluation) %>% dplyr::summarize(skill = "Reception", expSO = mean0(.data$point_won_by == .data$team)) %>% ungroup
        x <- left_join(x, lso, by = c("skill", "evaluation"))
        lbp <- refx %>% dplyr::filter(.data$skill == "Serve") %>% group_by(.data$evaluation) %>% dplyr::summarize(skill = "Serve", expBP = mean0(.data$point_won_by == .data$team)) %>% ungroup
        x <- left_join(x, lbp, by = c("skill", "evaluation"))
    } else {
        x$expBP <- x$expSO <- NA_real_
    }

    if (nrow(x) != starting_nrow)
        warning("data preprocessing has added rows: are there non-unique team or player identifiers?")

    ## perana_indoor or beach don't have setter calls
    if (!file_type %eq% "indoor") setter_calls <- NULL
    if (grepl("beach", file_type)) vote <- FALSE

    ## report icon image
    if (!is.null(icon)) icon <- normalizePath(icon, winslash = "/", mustWork = FALSE)
    ## cheap and nasty parameterisation
    vsx <- list(x = x, meta = meta, refx = refx, footnotes = footnotes, vote = vote, format = if (grepl("paged_", format)) "html" else format, style = style, shiny_progress = shiny_progress, file_type = file_type, icon = icon, css = css, remove_nonplaying = remove_nonplaying, base_font_size = 11)
    vsx <- c(vsx, dots) ## extra parms

    rm(x, meta, refx, vote, style, shiny_progress, file_type, icon, remove_nonplaying)

    ## generate report
    output_options <- NULL
    if (vsx$shiny_progress) try(shiny::setProgress(value = 0.1, message = "Generating report"), silent = TRUE)
    blah <- knitr::knit_meta(class = NULL, clean = TRUE) ## may help stop memory allocation error
    if (grepl("paged_", format)) {
        rgs <- list(input = rmd_template, output_file = outfile, output_options = list(self_contained = FALSE, copy_resources = TRUE), clean = TRUE)
        do.call(rmarkdown::render, rgs)
        rgs2 <- list(input = outfile, output = final_outfile, format = final_format)
        if (format == "paged_png") rgs2$scale <- 2
        if (length(chrome_print_extra_args) > 0) rgs2 <- c(rgs2, list(extra_args = chrome_print_extra_args))
        do.call(ovpaged::chrome_print, rgs2)
    } else {
        out <- render(rmd_template, output_file = outfile, output_options = output_options)
        if (final_format %in% c("pdf", "png")) {
            webshot::webshot(outfile, file = final_outfile)
            final_outfile
        } else {
            out
        }
    }
}

#' @export
#' @rdname vr_match_summary
vr_css <- function() {
    list(header_background = "#91A3B0", header_colour = "black", border = "1px solid black")
}

check_report_style <- function(style) {
    assert_that(is.string(style))
    style <- tolower(style)
    styles <- c("default", "ov1")
    if (!style %in% styles) stop("'style' should be one of: '", paste0(styles, collapse = "', '"), "'")
    style
}

#' Generate points table
#' @param x datavolleyplays: the \code{plays} component of an object as returned by \code{datavolley::read_dv}
#' @param team string: team name
#' @param by string: "player" or "set"
#' @param vote logical: if \code{TRUE}, include vote detail
#' @param style string: see [vr_match_summary()]
#' @export
vr_points <- function(x, team, by = "player", vote = FALSE, style = "default") {
    assert_that(is.string(by))
    by <- tolower(by)
    by <- match.arg(by, c("player", "set"))
    style <- check_report_style(style)
    assert_that(is.string(team))
    team_select <- team
    inc_set_errs <- style %in% c("ov1")
    inc_freeball_errs <- style %in% c("ov1")
    inc_block_errs <- style %in% c("ov1")
    blocked_is_err <- style %in% c("default")
    if (by == "player") {
        vr_pts <- x %>% dplyr::filter(.data$team %in% team_select, .data$player_id != "unknown player") %>% group_by(.data$player_id) %>%
            dplyr::summarize(Tot = sum(.data$evaluation_code == "#" & .data$skill %in% c("Serve", "Attack", "Block")),
                             BP = sum(.data$evaluation_code == "#" & .data$skill %in% c("Serve", "Attack", "Block") & .data$serving_team == team_select),
                             Nerr = sum((.data$evaluation %eq% "Error" & .data$skill %in% c("Serve", "Reception", "Attack", if (inc_set_errs) "Set", if (inc_freeball_errs) "Freeball")) | (inc_block_errs & .data$evaluation %eq% "Invasion" & .data$skill %eq% "Block") | (blocked_is_err & .data$evaluation %eq% "Blocked" & .data$skill %eq% "Attack")),
                             `W-L` = .data$Tot - .data$Nerr) %>%
            dplyr::select(-"Nerr")
        if (vote) {
            vr_pts <- left_join(vr_pts, vr_vote(x = x, team = team_select), by = "player_id")
        }
        vr_pts <- vr_pts %>%
            bind_rows(
                x %>% dplyr::filter(.data$team %in% team_select, .data$player_id != "unknown player") %>%
                mutate(player_id = "Team total") %>% 
                group_by(.data$player_id) %>%
                dplyr::summarize(Tot = sum(.data$evaluation_code == "#" & .data$skill %in% c("Serve", "Attack", "Block")),
                                 BP = sum(.data$evaluation_code == "#" & .data$skill %in% c("Serve", "Attack", "Block") & .data$serving_team == team_select),
                                 Nerr = sum((.data$evaluation %eq% "Error" & .data$skill %in% c("Serve", "Reception", "Attack", if (inc_set_errs) "Set", if (inc_freeball_errs) "Freeball")) | (inc_block_errs & .data$evaluation %eq% "Invasion" & .data$skill %eq% "Block") | (blocked_is_err & .data$evaluation %eq% "Blocked" & .data$skill %eq% "Attack")),
                                 `W-L` = .data$Tot - .data$Nerr) %>%
                dplyr::select(-"Nerr"))
        if (style %in% c("ov1")) vr_pts <- dplyr::select(vr_pts, -"BP", -"W-L")
    } else if (by == "set") {
        x$team_points <- if (team_select %eq% datavolley::home_team(x)) x$home_team_score else if (team_select %eq% datavolley::visiting_team(x)) x$visiting_team_score else NA_integer_
        vr_pts <- x %>% group_by(.data$set_number) %>%
            dplyr::summarize(Ser = sum(.data$evaluation_code == "#" & .data$skill == "Serve" & .data$team %in% team_select, na.rm = TRUE),
                             Atk = sum(.data$evaluation_code == "#" & .data$skill == "Attack" & .data$team %in% team_select, na.rm = TRUE),
                             Blo = sum(.data$evaluation_code == "#" & .data$skill == "Block" & .data$team %in% team_select, na.rm = TRUE),
                             "Op.Er" = suppressWarnings(max(.data$team_points, na.rm = TRUE)) - .data$Ser - .data$Atk - .data$Blo)
        vr_pts$`Op.Er`[is.infinite(vr_pts$`Op.Er`)] <- 0L ## if no points scores, max will return -Inf
    }
    vr_pts
}

#' Calculate vote
#' @param x datavolleyplays: the \code{plays} component of an object as returned by \code{datavolley::read_dv}
#' @param team string: team name
#' @export
vr_vote <- function(x, team) {
    as_for_datavolley <- TRUE
    assert_that(is.string(team))
    team_select <- team
    vote.df <- x %>% dplyr::filter(.data$team %in% team_select, .data$player_id != "unknown player") %>%
        mutate(vote_per_skill = case_when(.data$skill == "Serve" ~ case_when(.data$evaluation_code == "#" ~ 10,
                                                                             .data$evaluation_code == "+" ~ 7,
                                                                             .data$evaluation_code == "!" ~ if (as_for_datavolley) 0 else 5,
                                                                             .data$evaluation_code == "-" ~ 4,
                                                                             .data$evaluation_code == "/" ~ 8,
                                                                             .data$evaluation_code == "=" ~ 0),
                                          .data$skill == "Reception" ~ case_when(.data$evaluation_code == "#" ~ 10,
                                                                                 .data$evaluation_code == "+" ~ 7,
                                                                                 .data$evaluation_code == "!" ~ if (as_for_datavolley) 0 else 5,
                                                                                 .data$evaluation_code == "-" ~ if (as_for_datavolley) -1 else 1,
                                                                                 .data$evaluation_code == "/" ~ if (as_for_datavolley) -3 else -1,
                                                                                 .data$evaluation_code == "=" ~ -3),
                                          .data$skill == "Attack" ~ case_when(.data$evaluation_code == "#" ~ 10,
                                                                              .data$evaluation_code == "+" ~ if (as_for_datavolley) 5 else 7,
                                                                              .data$evaluation_code == "!" ~ if (as_for_datavolley) 0 else 5,
                                                                              .data$evaluation_code == "-" ~ if (as_for_datavolley) 5 else 3,
                                                                              .data$evaluation_code == "/" ~ 0,
                                                                              .data$evaluation_code == "=" ~ 0),
                                          .data$skill == "Block" ~ case_when(.data$evaluation_code == "#" ~ 10,
                                                                             .data$evaluation_code == "+" ~ 0,
                                                                             .data$evaluation_code == "!" ~ if (as_for_datavolley) 0 else 2,
                                                                             .data$evaluation_code == "-" ~ 0,
                                                                             .data$evaluation_code == "/" ~ 0,
                                                                             .data$evaluation_code == "=" ~ 0),
                                          .data$skill == "Dig" ~ case_when(.data$evaluation_code == "#" ~ 10,
                                                                           .data$evaluation_code == "+" ~ if (as_for_datavolley) 0 else 8,
                                                                           .data$evaluation_code == "!" ~ 0,
                                                                           .data$evaluation_code == "-" ~ 0,
                                                                           .data$evaluation_code == "/" ~ 0,
                                                                           .data$evaluation_code == "=" ~ 0),
                                          .data$skill == "Set" ~ case_when(.data$evaluation_code == "#" ~ 10,
                                                                           .data$evaluation_code == "+" ~ 7,
                                                                           .data$evaluation_code == "!" ~ 0,
                                                                           .data$evaluation_code == "-" ~ 0,
                                                                           .data$evaluation_code == "/" ~ 0,
                                                                           .data$evaluation_code == "=" ~ 0),
                                          .data$skill == "Freeball" ~ case_when(.data$evaluation_code == "#" ~ 10,
                                                                                .data$evaluation_code == "+" ~ if (as_for_datavolley) 0 else 5,
                                                                                .data$evaluation_code == "!" ~ 0,
                                                                                .data$evaluation_code == "-" ~ 0,
                                                                                .data$evaluation_code == "/" ~ 0,
                                                                                .data$evaluation_code == "=" ~ if (as_for_datavolley) 0 else -5)),
               max_vote_per_skill = case_when(.data$skill %in% c("Serve", "Reception", "Attack", "Block", "Dig", "Set", "Freeball") ~ 10)) %>%
        dplyr::select("team", "player_id", "skill", "evaluation_code", "vote_per_skill", "max_vote_per_skill") %>% na.omit()

    if (as_for_datavolley) {
        vote.df$factor <- rep(1.0, nrow(vote.df))
        team_totals <- dplyr::filter(x, .data$team %in% team_select)
        serve_grade <- vote.df %>% dplyr::filter(.data$skill == "Serve") %>%
            group_by_at("player_id") %>%
            dplyr::summarize(skill = "Serve", N = n(), vote = pmax(5.5, sum(.data$vote_per_skill*.data$factor)/n())) %>%
            mutate(vote = case_when(.data$N >= 0.05*sum(team_totals$skill == "Serve", na.rm = TRUE) ~ .data$vote))
        rec_grade <- vote.df %>% dplyr::filter(.data$skill == "Reception") %>%
            group_by_at("player_id") %>%
            dplyr::summarize(skill = "Reception", N = n(), vote = pmax(5.5, sum(.data$vote_per_skill*.data$factor)/n())) %>%
            mutate(vote = case_when(.data$N >= 0.12*sum(team_totals$skill == "Reception", na.rm = TRUE) ~ .data$vote))
        att_grade <- vote.df %>% dplyr::filter(.data$skill == "Attack") %>%
            group_by_at("player_id") %>%
            dplyr::summarize(skill = "Attack", N = n(), vote = pmax(5.5, sum(.data$vote_per_skill*.data$factor)/n())) %>%
            mutate(vote = case_when(.data$N >= 0.07*sum(team_totals$skill == "Attack", na.rm = TRUE) ~ .data$vote))
        block_grade <- team_totals %>% dplyr::filter(.data$skill == "Block" & .data$evaluation == "Winning block") %>% dplyr::count(.data$player_id)
        player_nsets <- bind_rows(lapply(block_grade$player_id, function(id) {
            ## count number of sets each player was listed as on court
            dplyr::tibble(player_id = id, n_sets_played = length(unique(na.omit(x$set_number[rowSums(x[, c(paste0("home_player_id", 1:6), paste0("visiting_player_id", 1:6))] == id) > 0]))))
        }))
        if (ncol(player_nsets) < 1) player_nsets <- tibble(player_id = character(), n_sets_played = integer())
        block_grade <- left_join(block_grade, player_nsets, by = "player_id") %>%
            mutate(skill = "Block", vote = case_when(.data$n >= .data$n_sets_played ~ 8.5,
                                                     .data$n >= .data$n_sets_played*0.8 ~ 8.0,
                                                     .data$n >= .data$n_sets_played*0.5 ~ 7.0)) %>%
            dplyr::select_at(c("player_id", "skill", "vote"))
        ## TODO setter, also include attacks after positive reception with weights error/blocked = 0, neg/pos = 5, kill = 10; only when those attacks are > 30% of number of team attacks
        bind_rows(serve_grade, rec_grade, att_grade, block_grade) %>% group_by_at("player_id") %>%
            dplyr::summarize(vote = round(mean0(.data$vote), 1))
    } else {
        vote.df %>% group_by_at("player_id") %>%
            dplyr::summarize(vote = round(sum(.data$vote_per_skill)/sum(.data$max_vote_per_skill)*10, 1), Nskills = n()) %>%
            mutate(vote = case_when(.data$Nskills < 10 ~ NA_real_, TRUE ~ .data$vote)) %>% dplyr::select(-"Nskills")
    }
}


#' Generate serve table
#' @param x datavolleyplays: the \code{plays} component of an object as returned by \code{datavolley::read_dv}
#' @param team string: team name
#' @param by string: "player" or "set"
#' @param refx data.frame: see [vr_match_summary()]
#' @param style string: see [vr_match_summary()]
#' @export
vr_serve <- function(x, team, by = "player", refx, style = "default"){
    assert_that(is.string(by))
    by <- match.arg(tolower(by), c("player", "set"))
    assert_that(is.string(team))
    team_select <- team
    style <- check_report_style(style)
    out <- if (by == "player") {
               x %>% dplyr::filter(.data$team %in% team_select, .data$player_id != "unknown player", .data$skill == "Serve") %>% group_by(.data$player_id) %>%
                   dplyr::summarize(Tot = n(),
                                    Err = sum(.data$evaluation %eq% "Error"),
                                    Pts = sum(.data$evaluation %eq% "Ace"),
                                    `srvEff%` = prc(round(serve_eff(.data$evaluation) * 100)),
                                    `expBP%` = prc(round(mean0(.data$expBP) * 100))) %>%
                   bind_rows(
                       x %>% dplyr::filter(.data$team %in% team_select, .data$player_id != "unknown player", .data$skill == "Serve") %>%
                       mutate(player_id = "Team total")%>% group_by(.data$player_id) %>%
                       dplyr::summarize(Tot = n(),
                                        Err = sum(.data$evaluation %eq% "Error"),
                                        Pts = sum(.data$evaluation %eq% "Ace"),
                                        `srvEff%` = prc(round(serve_eff(.data$evaluation) * 100)),
                                        `expBP%` = prc(round(mean0(.data$expBP) * 100)))
                   )
           } else if(by == "set") {
               x %>% dplyr::filter(.data$team %in% team_select, .data$skill == "Serve") %>% group_by(.data$set_number) %>%
                   dplyr::summarize(Tot = n(),
                                    Err = sum(.data$evaluation %eq% "Error"),
                                    Pts = sum(.data$evaluation %eq% "Ace"),
                                    `BP%` = prc(round(mean0(.data$point_won_by == .data$team) * 100)),
                                    `srvEff%` = prc(round(serve_eff(.data$evaluation) * 100)),
                                    `expBP%` = prc(round(mean0(.data$expBP) * 100)))
           }
    if (style %in% c("ov1")) {
        out <- if (is.null(refx)) dplyr::select(out, -"expBP%") else dplyr::select(out, -"srvEff%")
        dplyr::rename(out, Ace = "Pts")
    } else {
        out[, setdiff(names(out), c("BP%", "expBP%", "srvEff%")), drop = FALSE]
    }
}

#' Generate reception table
#' @param x datavolleyplays: the \code{plays} component of an object as returned by \code{datavolley::read_dv}
#' @param team string: team name
#' @param by string: "player" or "set"
#' @param refx data.frame: see [vr_match_summary()]
#' @param style string: see [vr_match_summary()]
#' @param file_type string: "indoor", "perana_indoor"
#' @export
vr_reception <- function(x, team, by = "player", refx, style = "default", file_type = "indoor"){
    assert_that(is.string(by))
    by <- match.arg(tolower(by), c("player", "set"))
    assert_that(is.string(team))
    team_select <- team
    style <- check_report_style(style)
    out <- if (by == "player"){
        x %>% dplyr::filter(.data$team %in% team_select, .data$player_id != "unknown player", .data$skill == "Reception") %>% group_by(.data$player_id) %>%
            dplyr::summarize(Tot = n(),
                             Err = sum(.data$evaluation %eq% "Error"),
                             `Pos%` = prc(round(mean0(.data$evaluation_code %in% c("+", "#", "#+")), 2)*100),
                             `(Exc%)` = prc(round(mean0(.data$evaluation_code %in% c("#")), 2)*100, before = "(", after = "%)"),
                             `recEff%` = prc(round(reception_eff(.data$evaluation) * 100)),
                             `expSO%` = prc(round(mean0(.data$expSO) * 100))) %>%
            bind_rows(
                x %>% dplyr::filter(.data$team %in% team_select, .data$player_id != "unknown player", .data$skill == "Reception") %>% 
                mutate(player_id = "Team total") %>%
                group_by(.data$player_id) %>%
                dplyr::summarize(Tot = n(),
                                 Err = sum(.data$evaluation %eq% "Error"),
                                 `Pos%` = prc(round(mean0(.data$evaluation_code %in% c("+", "#", "#+")), 2)*100),
                                 `(Exc%)` = prc(round(mean0(.data$evaluation_code %in% c("#")), 2)*100, before = "(", after = "%)"),
                                 `recEff%` = prc(round(reception_eff(.data$evaluation) * 100)),
                                 `expSO%` = prc(round(mean0(.data$expSO) * 100)))
            )
    } else if (by == "set") {
        x %>% dplyr::filter(.data$team %in% team_select, .data$skill == "Reception") %>% group_by(.data$set_number) %>%
            dplyr::summarize(Tot = n(),
                             Err = sum(.data$evaluation %eq% "Error"),
                             `Pos%` = prc(round(mean0(.data$evaluation_code %in% c("+", "#", "#+")), 2)*100),
                             `(Exc%)` = prc(round(mean0(.data$evaluation_code %in% c("#")), 2)*100, before = "(", after = "%)"),
                             `SO%` = prc(round(mean0(.data$point_won_by == .data$team) * 100)),
                             `recEff%` = prc(round(reception_eff(.data$evaluation) * 100)),
                             `expSO%` = prc(round(mean0(.data$expSO) * 100)))
    }
    if (style %in% c("ov1")) {
        out <- dplyr::select(out, -"(Exc%)")
        if (is.null(refx)) dplyr::select(out, -"expSO%") else dplyr::select(out, -"recEff%")
    } else {
        out[, setdiff(names(out), c("SO%", "expSO%", "recEff%")), drop = FALSE]
    }
}

#' Generate attack table
#' @param x datavolleyplays: the \code{plays} component of an object as returned by \code{datavolley::read_dv}
#' @param team string: team name
#' @param by string: "player" or "set"
#' @param style string: see [vr_match_summary()]
#' @export
vr_attack <- function(x, team, by = "player", style = "default") {
    assert_that(is.string(by))
    by <- match.arg(tolower(by), c("player", "set"))
    assert_that(is.string(team))
    team_select <- team
    style <- check_report_style(style)
    out <- if (by == "player") {
        x %>% dplyr::filter(.data$team %in% team_select, .data$player_id != "unknown player", .data$skill == "Attack") %>% group_by(.data$player_id) %>%
            dplyr::summarize(Tot = n(),
                      Err = sum(.data$evaluation %eq% "Error"),
                      Blo = sum(.data$evaluation %eq% "Blocked"),
                      `Pts` = sum(.data$evaluation %eq% "Winning attack"),
                      `Pts%` = prc(round(mean0(.data$evaluation %eq% "Winning attack"), 2)*100),
                      `attEff%` = prc(round(attack_eff(.data$evaluation) * 100))) %>%
            bind_rows(
                x %>% dplyr::filter(.data$team %in% team_select, .data$player_id != "unknown player", .data$skill == "Attack") %>%
                mutate(player_id = "Team total") %>%
                group_by(.data$player_id) %>%
                dplyr::summarize(Tot = n(),
                                 Err = sum(.data$evaluation %eq% "Error"),
                                 Blo = sum(.data$evaluation %eq% "Blocked"),
                                 `Pts` = sum(.data$evaluation %eq% "Winning attack"),
                                 `Pts%` = prc(round(mean0(.data$evaluation %eq% "Winning attack"), 2)*100),
                                 `attEff%` = prc(round(attack_eff(.data$evaluation) * 100)))
            )
           } else if (by == "set") {
               x %>% dplyr::filter(.data$team %in% team_select, .data$skill == "Attack") %>% group_by(.data$set_number) %>%
                   dplyr::summarize(Tot = n(),
                                    Err = sum(.data$evaluation %eq% "Error"),
                                    Blo = sum(.data$evaluation %eq% "Blocked"),
                                    `Pts` = sum(.data$evaluation %eq% "Winning attack"),
                                    `Pts%` = prc(round(mean0(.data$evaluation %in% "Winning attack"), 2)*100),
                                    `attEff%` = prc(round(attack_eff(.data$evaluation) * 100)))

           }
    if (style %in% c("ov1")) {
        out <- dplyr::rename(out, Kill = "Pts", "K%" = "Pts%")
    } else {
        out <- dplyr::select(out, -"attEff%")
    }
    out
}

#' Generate block table
#' @param x datavolleyplays: the \code{plays} component of an object as returned by \code{datavolley::read_dv}
#' @param team string: team name
#' @param by string: "player" or "set"
#' @param style string: see [vr_match_summary()]
#' @export
vr_block <- function(x, team, by = "player", style = "default") {
    assert_that(is.string(by))
    by <- match.arg(tolower(by), c("player", "set"))
    assert_that(is.string(team))
    team_select <- team
    style <- check_report_style(style)
    if (by == "player"){
        x %>% dplyr::filter(.data$team %in% team_select, .data$player_id != "unknown player") %>% group_by(.data$player_id) %>%
            dplyr::summarize(Tot = sum(.data$evaluation %eq% "Winning block" & .data$skill %eq% "Block")) %>%
            bind_rows(
                x %>% dplyr::filter(.data$team %in% team_select, .data$player_id != "unknown player") %>% 
                    mutate(player_id = "Team total") %>%
                    group_by(.data$player_id) %>%
                    dplyr::summarize(Tot = sum(.data$evaluation %eq% "Winning block" & .data$skill %eq% "Block"))
            )
    } else if (by == "set") {
        x %>% dplyr::filter(.data$team %in% team_select) %>% group_by(.data$set_number) %>%
            dplyr::summarize(Tot = sum(.data$evaluation %eq% "Winning block" & .data$skill %eq% "Block"))
    }
}
