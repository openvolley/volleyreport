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
