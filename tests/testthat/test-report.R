context("reporting")

test_that("minimal example report runs", {
    dvx <- datavolley::read_dv(datavolley::dv_example_file(1), insert_technical_timeouts=FALSE)
    rpt <- vr_match_summary(dvx, format = "html", vote = TRUE)
    rpt <- vr_match_summary(dvx, format = "html", vote = FALSE)
    rpt <- vr_match_summary(datavolley::dv_example_file(1), format = "pdf")
})
