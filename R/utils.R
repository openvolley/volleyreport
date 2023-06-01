`%eq%` <- function (x, y) x == y & !is.na(x) & !is.na(y)

single_value_or_na <- function(x) if (length(x) == 1) x else NA
single_value_or_na_char <- function(x) if (length(x) == 1) x else NA_character_
single_value_or_na_int <- function(x) if (length(x) == 1) x else NA_integer_

## mean but if empty vector passed, NA
mean0 <- function(x, ...) {
    if (length(x) < 1 || all(is.na(x))) as(NA, class(x)) else mean(x, na.rm = TRUE)
}

## division, avoiding div by 0 warnings (return NA not NaN)
`%/n/%` <- function(x, y) ifelse(abs(y) < 1e-09, NA_real_, x/y)

## convert to string with "%", but not if NA
prc <- function(z, before = "", after = "%") {
    if (length(z) < 1) z else ifelse(is.na(z), z, paste0(before, z, after))
}

## guess data type given plays data.frame
guess_data_type <- function(x) {
    if (!"home_player_id3" %in% names(x)) {
        if ("eventgrade" %in% names(x)) "perana_beach" else "beach"
    } else {
        if ("eventgrade" %in% names(x)) "perana_indoor" else "indoor"
    }
}

to_char_noNA <- function(z) {
    out <- as.character(z)
    out[is.na(out) | out %in% "NA"] <- ""
    out
}

cat0 <- function(...) cat(..., sep = "")

## webshot using phantomjs on recent Ubuntu/Debian will fail, because of openssl lib version issues
## workaround
safe_webshot <- function(...) {
    be_safe <- isTRUE(get_os() %in% c("linux", "unix"))
    if (be_safe) {
        temp <- Sys.getenv("OPENSSL_CONF")
        Sys.setenv(OPENSSL_CONF = "/dev/null")
    }
    out <- webshot::webshot(...)
    if (be_safe) Sys.setenv(OPENSSL_CONF = temp)
    out
}

get_os <- function() {
    if (.Platform$OS.type == "windows") return("windows")
    sysinf <- Sys.info()
    if (!is.null(sysinf)) {
        os <- sysinf["sysname"]
        if (tolower(os) == "darwin") os <- "osx"
    } else {
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os, ignore.case = TRUE)) os <- "osx"
        if (grepl("linux-gnu", R.version$os, ignore.case = TRUE)) os <- "linux"
    }
    os <- tolower(os)
    if (!os %in% c("windows", "linux", "unix", "osx")) warning("unknown operating system: ", os)
    os
}

beach_guess_roles_sides <- function(x, which_team = "home", threshold = 0.7) {
    ## threshold controls certainty - if we are uncertain, we won't give a player a side/role classification
    which_team <- match.arg(which_team, c("home", "visiting"))
    tmcol <- paste0(which_team, "_team")
    ## count blocks, and digs that were not immediately after a block touch
    bdx <- x %>% dplyr::filter(.data$team == .data[[tmcol]], .data$skill == "Block") %>% dplyr::count(.data$player_id, name = "n_blocks") %>% mutate(prop_block = .data$n_blocks / sum(.data$n_blocks)) %>%
        full_join(x %>% dplyr::filter(.data$team == .data[[tmcol]], .data$skill == "Dig", lag(.data$skill) != "Block") %>% dplyr::count(.data$player_id, name = "n_digs") %>% mutate(prop_dig = .data$n_digs / sum(.data$n_digs)), by = "player_id") %>%
        mutate(prop_block = if_else(is.na(.data$prop_block), 0, .data$prop_block), prop_dig = if_else(is.na(.data$prop_dig), 0, .data$prop_dig),
               bd = (.data$prop_block + 1 - .data$prop_dig) / 2)
    lrx <- x %>% dplyr::filter(.data$team == .data[[tmcol]], .data$skill %in% c("Reception")) %>% ##, "Attack")) %>% ## don't use attacks, bit ambiguous
        mutate(left = (.data$skill == "Attack" & (.data$start_zone %in% c(4, 7, 5) | .data$start_coordinate_x < 1.5)) |
                   (.data$skill == "Reception" & (.data$end_zone %in% c(4, 7, 5) | .data$end_coordinate_x < 1.5)),
               right = (.data$skill == "Attack" & (.data$start_zone %in% c(2, 9, 1) | .data$start_coordinate_x > 2.5)) |
                   (.data$skill == "Reception" & (.data$end_zone %in% c(2, 9, 1) | .data$end_coordinate_x > 2.5))) %>%
        group_by(.data$player_id) %>% dplyr::summarize(n_left = sum(.data$left, na.rm = TRUE), n_right = sum(.data$right, na.rm = TRUE),
                                                         prop_left = .data$n_left / (.data$n_left + .data$n_right),
                                                         prop_right = .data$n_right / (.data$n_left + .data$n_right))

    bdx <- bdx %>% full_join(lrx, by = "player_id") %>% dplyr::filter(!is.na(.data$player_id), !.data$player_id %in% bdx$player_id[duplicated(bdx$player_id)])
    if (nrow(bdx) == 2) {
        bdx <- bdx %>% mutate(player_beach_side = case_when(.data$prop_left > threshold ~ "L",
                                                            .data$prop_right > threshold ~ "R"),
                              player_beach_role = case_when(.data$bd > threshold ~ "Blk",
                                                            .data$bd < (1 - threshold) ~ "Def"))
        ## since this is a single match, if we have inferred left/right for one player and not the other, take the complement
        if (sum(is.na(bdx$player_beach_side)) == 1) {
            nidx <- is.na(bdx$player_beach_side)
            if (isTRUE(bdx$player_beach_side[!nidx] == "L" && bdx$prop_right[nidx] > 0.5)) {
                bdx$player_beach_side[nidx] <- "R"
            } else if (isTRUE(bdx$player_beach_side[!nidx] == "R" && bdx$prop_left[nidx] > 0.5)) {
                bdx$player_beach_side[nidx] <- "L"
            }
        }
        ## ditto role? not sure about this yet
        ##    if (sum(is.na(bdx$player_beach_role)) == 1) bdx$player_beach_role[is.na(bdx$player_beach_role)] <- setdiff(c("Blocker", "Defender", NA_character_), bdx$player_beach_role)
        ## catastrophe check, don't allow duplicates
        if (any(duplicated(bdx$player_beach_side))) player_beach_side <- NA_character_
        if (any(duplicated(bdx$player_beach_role))) player_beach_role <- NA_character_
    } else {
        bdx$player_beach_side <- bdx$player_beach_role <- NA_character_
    }
    bdx %>% dplyr::select("player_id", "player_beach_side", "player_beach_role") %>% dplyr::filter(!is.na(.data$player_id), !.data$player_id %in% bdx$player_id[duplicated(bdx$player_id)])
}
