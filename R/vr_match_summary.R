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
#' @param base_font_size numeric: the base font size (the font sizes in different parts of the report are scaled relative to this)
#' @param court_plots_function string or function: a function, or name of a function, that takes a datavolley object and produces a plot object. Supply your own function here to override the court plots that are included in the report for some values of `style`
#' @param court_plots_args list: named list of arguments to pass to the court plot function
#' @param plot_icons logical or data.frame: some values of `style` will include plots of various kinds in the report. Currently `plot_icons` defaults to `TRUE` for `style = "ov1"` on a beach match, otherwise `FALSE` (plot icons generally tend to be visually distracting with indoor, particularly the error icons). Set `plot_icons` to `FALSE` for no icons, `TRUE` to use the icons specified by [vr_plot_icons()], or a data.frame as returned by [vr_plot_icons()] to control the icons that will be used. Note that only (free) fontawesome icons are supported
#' @param skill_evaluation_decode : as for [datavolley::dv_read()]
#' @param shiny_progress logical: if \code{TRUE}, the report generation process will issue \code{shiny::setProgress()} calls. The call to \code{vr_match_summary} should therefore be wrapped in a \code{shiny::withProgress()} scope
#' @param single_page_tries integer: experimental! Ideally we want a single-page report, but until the report is rendered to PDF we don't know for sure whether it will fit on one page. If `single_page_tries` is greater than 1, we will try re-rendering the report (trying up to this many times). If it does not fit on a single page, the `base_font_size` will be progressively reduced on each try. Note that this only applies to `format` "paged_pdf"
#' @param chrome_print_extra_args character: additional parameters to pass as `extra_args` to [pagedown::chrome_print()] (only relevant if using a "paged_*" format)
#' @param ... : additional parameters passed to the rmarkdown template
#' @return The path to the report file
#'
#' @examples
#' \dontrun{
#'   f <- vr_match_summary(dv_example_file(), format = "paged_pdf")
#'   if (interactive()) browseURL(f)
#' }
#' @export
vr_match_summary <- function(x, outfile, refx, vote = TRUE, format = "html", icon = NULL, css = vr_css(), remove_nonplaying = TRUE, style = "default", base_font_size = 11, court_plots_function = "vr_court_plots", court_plots_args = list(), plot_icons, skill_evaluation_decode = "guess", single_page_tries = 1L, shiny_progress = FALSE, chrome_print_extra_args = NULL, ...) {
    if (is.string(x) && file.exists(x)) {
        if (grepl("\\.(dvw|vsm|xml)$", x, ignore.case = TRUE)) {
            x <- datavolley::dv_read(x, skill_evaluation_decode = skill_evaluation_decode)
        } else {
            stop("unknown file format: ", x)
        }
    }
    assert_that(inherits(x, c("datavolley", "peranavolley")), msg = "x should be a datavolley object")
    assert_that(is.string(format))
    assert_that(is.flag(shiny_progress), !is.na(shiny_progress))
    format <- tolower(format)
    format <- match.arg(format, c("html", "pdf", "png", "paged_pdf", "paged_png", "paged_html"))
    if (format %in% c("pdf", "png")) {
        ## check that we have phantomjs installed
        if (!webshot::is_phantomjs_installed()) {
            stop("phantomjs must be installed for pdf/png format. See help('install_phantomjs', 'webshot') or consider using paged_", format, " format")
        }
    }
    style <- check_report_style(style)
    ## court_plots_function is either a user-defind function, or more likely the vr_court_plots function in this package
    court_plots_function <- tryCatch(match.fun(court_plots_function), error = function(e) {
        tryCatch(get(court_plots_function, envir = asNamespace("volleyreport"), mode = "function", inherits = FALSE), error = function(e) {
            stop("could not match court_plots_function to a function")
        })
    })
    if (!is.list(court_plots_args) || length(names(court_plots_args)) != length(court_plots_args) || any(is.na(names(court_plots_args)) | !nzchar(names(court_plots_args)))) {
        warning("court_plots_args should be a named list, ignoring")
        court_plots_args <- list()
    }

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
            } else if (!((is.data.frame(refx) && nrow(refx) > 0) || (is.list(refx) && setequal(tolower(names(refx)), c("expso", "expbp"))))) {
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
    if (!grepl("paged_", format)) format <- "html" ## for pdf or png, treat now as html then webshot to pdf/png from that
    working_dir <- tempfile()
    dir.create(working_dir)
    rmd_template <- file.path(working_dir, paste0(if (grepl("paged", format)) "paged_pdf" else format, ".Rmd"))
    if (!file.copy(from = system.file(file.path("extdata", paste0(if (grepl("paged", format)) "paged_pdf" else format, ".Rmd")), package = "volleyreport"), to = rmd_template))
        stop("cannot copy template file to temporary directory")

    if (final_format %in% c("pdf", "png")) {
        final_outfile <- if (missing(outfile)) tempfile(fileext = paste0(".", final_format)) else outfile
        outfile <- tempfile(fileext = ".html")
    } else {
        if (missing(outfile)) outfile <- tempfile(fileext = paste0(".", sub("paged_", "", format)))
    }
    file_type <- NULL ## indoor (datavolley) or perana_indoor or potentially beach
    if ("file_meta" %in% names(x)) file_type <- x$file_meta$file_type
    x <- datavolley::plays(x)
    if (!"home_score_start_of_point" %in% names(x)) {
        x$home_score_start_of_point <- ifelse(x$point_won_by %eq% x$home_team, as.integer(x$home_team_score - 1L), as.integer(x$home_team_score))
        x$visiting_score_start_of_point <- ifelse(x$point_won_by %eq% x$visiting_team, as.integer(x$visiting_team_score - 1L), as.integer(x$visiting_team_score))
    }
    ## fix a legacy issue where home_score_start_of_point and visiting_score_start_of_point had incorrect NAs for some types of file
    temp <- x %>% group_by(.data$point_id) %>% dplyr::summarize(home_score_start_of_point = single_unique_value_or_na_int(.data$home_score_start_of_point),
                                                                visiting_score_start_of_point = single_unique_value_or_na_int(.data$visiting_score_start_of_point)) %>% ungroup
    x <- x %>% dplyr::select(-"home_score_start_of_point", -"visiting_score_start_of_point") %>% left_join(temp, by = "point_id")
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
    if (!"phase" %in% names(x)) x$phase <- datavolley::play_phase(x)
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
        x <- mutate(x, freeball_over = .data$skill %eq% "Freeball" & lag(.data$match_id) %eq% .data$match_id & lag(.data$point_id) %eq% .data$point_id & ((!is.na(lead(.data$team)) & lead(.data$team) != .data$team) | lag(.data$team) %eq% .data$team))
    }

    starting_nrow <- nrow(x)
    if (!"ts_pass_evaluation_code" %in% names(x)) {
        touchsum <- x %>% ungroup %>% dplyr::filter(!is.na(.data$team)) %>% group_by(.data$match_id, .data$team, .data$team_touch_id) %>%
            dplyr::summarize(ts_pass_evaluation_code = single_value_or_na_char(na.omit(.data$evaluation_code[.data$skill %in% c("Reception", "Dig") | (.data$skill %eq% "Freeball" & !.data$freeball_over)]))) %>%
            ungroup %>% dplyr::select(-"team")
        x <- left_join(x, touchsum, by = c("match_id", "team_touch_id"))
    }

    if (!is.null(refx)) {
        srmap <- dplyr::tribble(
            ~skill, ~evaluation_code, ~evaluation,
            "Serve", "=", "Error",
            "Serve", "/", "Positive, no attack",
            "Serve", "-", "Negative, opponent free attack",
            "Serve", "+", "Positive, opponent some attack",
            "Serve", "#", "Ace",
            "Serve", "!", "OK, no first tempo possible",
            "Reception", "=", "Error",
            "Reception", "/", "Poor, no attack",
            "Reception", "-", "Negative, limited attack",
            "Reception", "-/", "Negative/poor pass",
            "Reception", "+", "Positive, attack",
            "Reception", "#", "Perfect pass",
            "Reception", "#+", "Perfect/positive pass",
            "Reception", "!", "OK, no first tempo possible")
        ## deal with the various forms that refx can take
        if (is.list(refx) && setequal(tolower(names(refx)), c("expso", "expbp"))) {
            ## refx is of form list(expSO = list(`R#` = 0.7, ...), expBP = list(...)) or list(expSO = tibble(...), expBP = tibble(...)) following lso and lbp
            names(refx) <- tolower(names(refx))
            ## expSO
            lso <- if (is.data.frame(refx$expso)) refx$expso else tibble(skill = "Reception", evaluation = names(refx$expso), expSO = as.numeric(unlist(refx$expso)))
            if (all(grepl("R?[[:punct:]]", lso$evaluation))) {
                lso <- mutate(lso, evaluation = sub("^R", "", .data$evaluation)) %>% dplyr::rename(evaluation_code = "evaluation") %>% left_join(srmap, by = c("skill", "evaluation_code")) %>% dplyr::select(-"evaluation_code")
            }
            lbp <- if (is.data.frame(refx$expbp)) refx$expbp else tibble(skill = "Serve", evaluation = names(refx$expbp), expBP = as.numeric(unlist(refx$expbp)))
            if (all(grepl("S?[[:punct:]]", lbp$evaluation))) {
                lbp <- mutate(lbp, evaluation = sub("^S", "", .data$evaluation)) %>% dplyr::rename(evaluation_code = "evaluation") %>% left_join(srmap, by = c("skill", "evaluation_code")) %>% dplyr::select(-"evaluation_code")
            }
            lso <- distinct(na.omit(lso))
            lbp <- distinct(na.omit(lbp))
            if (sum(lso$evaluation == "Error") < 1) lso <- bind_rows(lso, list(skill = "Reception", evaluation = "Error", expSO = 0))
            if (sum(lbp$evaluation == "Error") < 1) lbp <- bind_rows(lbp, list(skill = "Serve", evaluation = "Error", expBP = 0))
            if (sum(lbp$evaluation == "Ace") < 1) lbp <- bind_rows(lbp, list(skill = "Serve", evaluation = "Ace", expBP = 1))
        } else {
            lso <- refx %>% dplyr::filter(.data$skill == "Reception" & !is.na(.data$evaluation)) %>% group_by(.data$evaluation) %>% dplyr::summarize(skill = "Reception", expSO = mean0(.data$point_won_by == .data$team)) %>% ungroup
            lbp <- refx %>% dplyr::filter(.data$skill == "Serve" & !is.na(.data$evaluation)) %>% group_by(.data$evaluation) %>% dplyr::summarize(skill = "Serve", expBP = mean0(.data$point_won_by == .data$team)) %>% ungroup


##            if (FALSE) {
##                fx <- refx %>% dplyr::filter(skill == "Attack") %>% mutate(kill = evaluation == "Winning attack", phase = as.factor(phase), skill_type = as.factor(skill_type), start_zone = as.factor(start_zone), attack_code = as.factor(attack_code))
##
##                fit1 <- gam(kill ~ s(start_zone, bs = "re") + s(skill_type, bs = "re") + s(phase, bs = "re"), family = binomial, data = fx)
##
##                fit1 <- gam(kill ~ s(phase, attack_code, bs = "re"), family = binomial, data = fx)
##                data.frame(what = levels(interaction(levels(fx$phase), levels(fx$attack_code))), coef = coef(fit1)[-1])
##            }


        }
        ## after all that, check that lso and lbp are ok
        l_ok <- is.data.frame(lso) && is.data.frame(lbp) &&
            setequal(names(lso), c("skill", "evaluation", "expSO")) && setequal(names(lbp), c("skill", "evaluation", "expBP")) &&
            !any(duplicated(lso$evaluation)) && !any(duplicated(lbp$evaluation)) &&
            all(lso$skill == "Reception") && all(lbp$skill == "Serve")
        if (!l_ok) {
            warning("reference data does not look correct, ignoring")
            x$expBP <- x$expSO <- NA_real_
        } else {
            x <- x[, setdiff(names(x), c("expSO", "expBP")), drop = FALSE]
            x <- left_join(x, lso, by = c("skill", "evaluation"))
            x <- left_join(x, lbp, by = c("skill", "evaluation"))
        }
    } else {
        x$expBP <- x$expSO <- NA_real_
    }

    if (nrow(x) != starting_nrow) warning("data preprocessing has added rows: are there non-unique team or player identifiers?")

    ## perana_indoor or beach don't have setter calls
    if (!file_type %eq% "indoor") setter_calls <- NULL
    if (grepl("beach", file_type)) vote <- FALSE

    ## report icon image
    if (!is.null(icon)) icon <- normalizePath(icon, winslash = "/", mustWork = FALSE)
    ## other plot icons
    if (missing(plot_icons)) plot_icons <- style %in% c("ov1") && grepl("beach", file_type)
    use_plot_icons <- is.data.frame(plot_icons) || (is.logical(plot_icons) && isTRUE(plot_icons))
    if (!is.data.frame(plot_icons)) plot_icons <- vr_plot_icons()
    ## so use_plot_icons tells us whether to include plot icons, and plot_icons are the actual icons (in a df)
    if (style %in% c("ov1")) {
        ## include the plot summary stats by block?
        plotsum <- if (grepl("beach", file_type)) {
                       TRUE
                   } else {
                       ## if we have a full page, we have to omit them and just show the score evolution bars
                       nhp <- sum(apply(meta$players_h[, grep("starting_position", names(meta$players_h))], 1, function(z) !all(is.na(z))))
                       nvp <- sum(apply(meta$players_v[, grep("starting_position", names(meta$players_v))], 1, function(z) !all(is.na(z))))
                       nsets <- sum(meta$teams$sets_won)
                       ht <- ((nhp * 4 + 18) + (nvp * 4 + 18) + (4.5 * nsets + 8.5)) ## kind of a height measure of the team tables + set summaries
                       ##message("PS: ", nhp, " + ", nvp, " + ", nsets, " = ", ht)
                       ht < 160 ## rule of thumb, more than this and the plot won't fit
                   }
    } else {
        plotsum <- FALSE
    }
    ## cheap and nasty parameterisation
    vsx <- list(x = x, meta = meta, refx = refx, footnotes = footnotes, vote = vote, format = if (grepl("paged_", format)) "html" else format, style = style,
                shiny_progress = shiny_progress, file_type = file_type, icon = icon, css = css, remove_nonplaying = remove_nonplaying, base_font_size = base_font_size,
                plot_summary = plotsum, plot_icons = plot_icons, use_plot_icons = use_plot_icons, court_plots_fun = court_plots_function, court_plots_args = court_plots_args)
    vsx <- c(vsx, dots) ## extra parms

    rm(x, meta, refx, vote, style, shiny_progress, file_type, icon, remove_nonplaying)

    ## generate report
    output_options <- NULL
    if (isTRUE(vsx$plot_summary) || isTRUE(vsx$use_plot_icons)) showtext::showtext_auto()
    max_tries <- if (format == "paged_pdf" && isTRUE(single_page_tries > 1)) single_page_tries else 1L
    for (this_try in seq_len(max_tries)) {
        vsx$base_font_size <- base_font_size * (0.95^(this_try - 1)) ## progressively smaller font
        if (vsx$shiny_progress) try(shiny::setProgress(value = 0.1, message = "Generating report"), silent = TRUE)
        blah <- knitr::knit_meta(class = NULL, clean = TRUE) ## may help stop memory allocation error
        f <- if (format == "paged_html") {
                 rgs <- list(input = rmd_template, output_file = outfile, output_options = list(self_contained = TRUE), clean = TRUE)
                 do.call(rmarkdown::render, rgs)
             } else if (grepl("paged_", format)) {
                 rgs <- list(input = rmd_template, output_file = outfile, output_options = list(self_contained = FALSE, copy_resources = TRUE), clean = TRUE)
                 do.call(rmarkdown::render, rgs)
                 rgs2 <- list(input = outfile, output = final_outfile, format = final_format)
                 if (format == "paged_png") rgs2$scale <- 2
                 if (length(chrome_print_extra_args) > 0) rgs2 <- c(rgs2, list(extra_args = chrome_print_extra_args))
                 do.call(ovpaged::chrome_print, rgs2)
             } else {
                 out <- render(rmd_template, output_file = outfile, output_options = output_options)
                 if (final_format %in% c("pdf", "png")) {
                     safe_webshot(outfile, file = final_outfile)
                     final_outfile
                 } else {
                     out
                 }
             }
        if (this_try < max_tries) {
            if (tryCatch(pdftools::pdf_info(f)$pages < 2, error = function(e) TRUE)) break ## bail out if we've achieved a single page
        }
    }
    if (isTRUE(vsx$plot_summary) || isTRUE(vsx$use_plot_icons)) showtext::showtext_auto(enable = FALSE)
    f
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
#' @param x datavolleyplays: the \code{plays} component of an object as returned by \code{datavolley::dv_read}
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
        vr_pts <- x %>% dplyr::filter(!is.na(.data$set_number)) %>% group_by(.data$set_number) %>%
            dplyr::summarize(Ser = sum(.data$evaluation_code == "#" & .data$skill == "Serve" & .data$team %in% team_select, na.rm = TRUE),
                             Atk = sum(.data$evaluation_code == "#" & .data$skill == "Attack" & .data$team %in% team_select, na.rm = TRUE),
                             Blo = sum(.data$evaluation_code == "#" & .data$skill == "Block" & .data$team %in% team_select, na.rm = TRUE),
                             "Op.Er" = suppressWarnings(max(.data$team_points, na.rm = TRUE)) - .data$Ser - .data$Atk - .data$Blo)
        vr_pts$`Op.Er`[is.infinite(vr_pts$`Op.Er`)] <- 0L ## if no points scores, max will return -Inf
    }
    vr_pts
}

#' Calculate vote
#' @param x datavolleyplays: the \code{plays} component of an object as returned by \code{datavolley::dv_read}
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
            group_by(.data$player_id) %>%
            dplyr::summarize(skill = "Serve", N = n(), vote = pmax(5.5, sum(.data$vote_per_skill*.data$factor)/n())) %>%
            mutate(vote = case_when(.data$N >= 0.05*sum(team_totals$skill == "Serve", na.rm = TRUE) ~ .data$vote))
        rec_grade <- vote.df %>% dplyr::filter(.data$skill == "Reception") %>%
            group_by(.data$player_id) %>%
            dplyr::summarize(skill = "Reception", N = n(), vote = pmax(5.5, sum(.data$vote_per_skill*.data$factor)/n())) %>%
            mutate(vote = case_when(.data$N >= 0.12*sum(team_totals$skill == "Reception", na.rm = TRUE) ~ .data$vote))
        att_grade <- vote.df %>% dplyr::filter(.data$skill == "Attack") %>%
            group_by(.data$player_id) %>%
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
        bind_rows(serve_grade, rec_grade, att_grade, block_grade) %>% group_by(.data$player_id) %>%
            dplyr::summarize(vote = round(mean0(.data$vote), 1))
    } else {
        vote.df %>% group_by(.data$player_id) %>%
            dplyr::summarize(vote = round(sum(.data$vote_per_skill)/sum(.data$max_vote_per_skill)*10, 1), Nskills = n()) %>%
            mutate(vote = case_when(.data$Nskills < 10 ~ NA_real_, TRUE ~ .data$vote)) %>% dplyr::select(-"Nskills")
    }
}


#' Generate serve table
#' @param x datavolleyplays: the \code{plays} component of an object as returned by \code{datavolley::dv_read}
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
#' @param x datavolleyplays: the \code{plays} component of an object as returned by \code{datavolley::dv_read}
#' @param team string: team name
#' @param by string: "player" or "set"
#' @param refx data.frame: see [vr_match_summary()]
#' @param style string: see [vr_match_summary()]
#' @param file_type string: "indoor", "perana_indoor", "beach", "perana_beach"
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
#' @param x datavolleyplays: the \code{plays} component of an object as returned by \code{datavolley::dv_read}
#' @param team string: team name
#' @param by string: "player" or "set"
#' @param file_type string: "indoor", "perana_indoor", "beach", "perana_beach"
#' @param style string: see [vr_match_summary()]
#' @export
vr_attack <- function(x, team, by = "player", file_type = "indoor", style = "default") {
    assert_that(is.string(by))
    by <- match.arg(tolower(by), c("player", "set"))
    assert_that(is.string(team))
    team_select <- team
    style <- check_report_style(style)
    assert_that(is.string(file_type))
    ## 2nd-ball attack. Don't include attacks after a block touch
    x <- x %>% mutate(attack_followed_block_touch = .data$skill == "Attack" &
                          ((lag(.data$skill == "Block") & lag(.data$team) == .data$team) | ## attack immediately after block touch
                           (grepl("beach", file_type) & lag(.data$skill == "Block", 2) & lag(.data$team, 2) == .data$team & lag(.data$team) == .data$team)), ## block touch then another touch (e.g. set or dig) then attack
                      on2 = !.data$attack_followed_block_touch & .data$skill == "Attack" &
                          ((lag(.data$skill) %in% c("Reception", "Dig") & lag(.data$team) == .data$team & !is.na(.data$player_id) & .data$player_id != lag(.data$player_id)) |
                           ## allow for common 2nd touch entries in the attack description (will help if e.g. digs not scouted)
                           grepl("\\b(2nd Touch|Second touch|2TAttack|On 2)\\b", .data$attack_description, ignore.case = TRUE)))
    chk <- x %>% dplyr::summarize(n_trans_digs_scouted = sum(.data$skill == "Dig" & .data$phase == "Transition" & .data$evaluation != "Error", na.rm = TRUE),
                                  n_trans_attacks_scouted = sum(.data$skill == "Attack" & .data$phase == "Transition", na.rm = TRUE))
    out <- if (by == "player") {
        x %>% dplyr::filter(.data$team %in% team_select, .data$player_id != "unknown player", .data$skill == "Attack") %>% group_by(.data$player_id) %>%
            dplyr::summarize(Tot = n(),
                      Err = sum(.data$evaluation %eq% "Error"),
                      Blo = sum(.data$evaluation %eq% "Blocked"),
                      `Pts` = sum(.data$evaluation %eq% "Winning attack"),
                      `Pts%` = prc(round(mean0(.data$evaluation %eq% "Winning attack"), 2)*100),
                      `attEff%` = prc(round(attack_eff(.data$evaluation) * 100)),
                      On2 = sum(.data$on2, na.rm = TRUE),
                      `On2 K%` = as.character(prc(round(sum(.data$evaluation %eq% "Winning attack" & .data$on2, na.rm = TRUE) %/n/% sum(.data$on2, na.rm = TRUE), 2)*100))) %>%
            bind_rows(
                x %>% dplyr::filter(.data$team %in% team_select, .data$player_id != "unknown player", .data$skill == "Attack") %>%
                mutate(player_id = "Team total") %>%
                group_by(.data$player_id) %>%
                dplyr::summarize(Tot = n(),
                                 Err = sum(.data$evaluation %eq% "Error"),
                                 Blo = sum(.data$evaluation %eq% "Blocked"),
                                 `Pts` = sum(.data$evaluation %eq% "Winning attack"),
                                 `Pts%` = prc(round(mean0(.data$evaluation %eq% "Winning attack"), 2)*100),
                                 `attEff%` = prc(round(attack_eff(.data$evaluation) * 100)),
                                 On2 = sum(.data$on2, na.rm = TRUE),
                                 `On2 K%` = as.character(prc(round(sum(.data$evaluation %eq% "Winning attack" & .data$on2, na.rm = TRUE) %/n/% sum(.data$on2, na.rm = TRUE), 2)*100)))
            )
           } else if (by == "set") {
               x %>% dplyr::filter(.data$team %in% team_select, .data$skill == "Attack") %>% group_by(.data$set_number) %>%
                   dplyr::summarize(Tot = n(),
                                    Err = sum(.data$evaluation %eq% "Error"),
                                    Blo = sum(.data$evaluation %eq% "Blocked"),
                                    `Pts` = sum(.data$evaluation %eq% "Winning attack"),
                                    `Pts%` = prc(round(mean0(.data$evaluation %in% "Winning attack"), 2)*100),
                                    `attEff%` = prc(round(attack_eff(.data$evaluation) * 100)),
                                    On2 = sum(.data$on2, na.rm = TRUE),
                                    `On2 K%` = as.character(prc(round(sum(.data$evaluation %eq% "Winning attack" & .data$on2, na.rm = TRUE) %/n/% sum(.data$on2, na.rm = TRUE), 2)*100)))

           }
    if (!(grepl("beach", file_type) && style %in% c("ov1"))) {
        out <- dplyr::select(out, -"On2", -"On2 K%")
    } else {
        ## also exclude the On2 columns if digs haven't been consistently scouted (assume receptions have)
        if (chk$n_trans_digs_scouted / chk$n_trans_attacks_scouted < 0.3) out <- dplyr::select(out, -"On2", -"On2 K%")
    }
    if (style %in% c("ov1")) {
        out <- dplyr::rename(out, Kill = "Pts", "K%" = "Pts%")
    } else {
        out <- dplyr::select(out, -"attEff%")
    }
    out
}

#' Generate block table
#' @param x datavolleyplays: the \code{plays} component of an object as returned by \code{datavolley::dv_read}
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
