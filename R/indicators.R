## copied from ovlytics

## Various skill performance indicators
##
## * attack_eff: (number of kills - number of errors and blocked attacks) / (number of attacks)
## * serve_eff: (number of aces and positive serves - number of errors and poor serves) / (number of serves)
## * reception_eff: (number of perfect and positive passes - number of errors and overpasses) / (number of passes)
##
## @param evaluation character: vector of skill evaluations ("Winning attack", "Error", etc)
## @param skill character: (optional) vector of skill values ("Attack", "Block", etc). If provided, it will be used to filter the `evaluation` vector to the elements corresponding to the correct skill. If not provided, all elements of `evaluation` will be used
##
## @return A numeric scalar
## @examples
## \dontrun{
##  library(dplyr)
##  x <- ovdata_example("mlafin_braslovce_nkbm", as = "parsed")
##  plays(x) %>% dplyr::filter(skill == "Attack") %>% group_by(player_name) %>%
##    dplyr::summarize(N_attacks = n(), att_eff = attack_eff(evaluation))
## }
##
## @export
attack_eff <- function(evaluation, skill) {
    assert_that(is.character(evaluation))
    if (!missing(skill)) {
        assert_that(is.character(skill))
        evaluation <- evaluation[skill %eq% "Attack"]
    }
    (sum(evaluation %eq% "Winning attack") - sum(evaluation %in% c("Error", "Blocked"))) / sum(!is.na(evaluation))
}

## @rdname attack_eff
## @export
serve_eff <- function(evaluation, skill) {
    assert_that(is.character(evaluation))
    if (!missing(skill)) {
        assert_that(is.character(skill))
        evaluation <- evaluation[skill %eq% "Serve"]
    }
    (sum(grepl("^(Ace|Positive)", evaluation)) - sum(grepl("^(Error|Negative)", evaluation))) / sum(!is.na(evaluation))
}

## @rdname attack_eff
## @export
reception_eff <- function(evaluation, skill) {
    assert_that(is.character(evaluation))
    if (!missing(skill)) {
        assert_that(is.character(skill))
        evaluation <- evaluation[skill %eq% "Reception"]
    }
    (sum(grepl("^(Perfect|Positive)", evaluation)) - sum(grepl("^(Error|Poor)", evaluation))) / sum(!is.na(evaluation))
}

