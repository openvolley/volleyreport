context("reporting")

dvx <- datavolley::read_dv(datavolley::dv_example_file(1), insert_technical_timeouts=FALSE)

test_that("minimal HTML example report runs", {
    skip_if_not(rmarkdown::pandoc_available(version = "1.12.3"))
    rpt <- vr_match_summary(dvx, format = "html", vote = TRUE)
    rpt <- vr_match_summary(dvx, format = "html", vote = FALSE)
})

test_that("minimal PDF example report runs", {
    skip_if_not(rmarkdown::pandoc_available(version = "1.12.3"))
    skip_if_not(webshot::is_phantomjs_installed())
    rpt <- vr_match_summary(datavolley::dv_example_file(1), format = "pdf")
})

test_that("variety of example reports run", {
    skip_if_not(rmarkdown::pandoc_available(version = "1.12.3"))
    rpt <- vr_match_summary(datavolley::dv_example_file(1), format = "html", remove_nonplaying = FALSE)
    rpt <- vr_match_summary(datavolley::dv_example_file(1), format = "paged_pdf")
    rpt <- vr_match_summary(datavolley::dv_example_file(1), format = "paged_pdf", vote = FALSE)
    rpt <- vr_match_summary(datavolley::dv_example_file(1), format = "paged_pdf", style = "ov1")
})
