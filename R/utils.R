`%eq%` <- function (x, y) x == y & !is.na(x) & !is.na(y)

single_value_or_na <- function(x) if (length(x) == 1) x else NA
single_value_or_na_char <- function(x) if (length(x) == 1) x else NA_character_
single_value_or_na_int <- function(x) if (length(x) == 1) x else NA_integer_

## mean but if empty vector passed, NA
mean0 <- function(x, ...) {
    if (length(x) < 1 || all(is.na(x))) as(NA, class(x)) else mean(x, na.rm = TRUE)
}

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
