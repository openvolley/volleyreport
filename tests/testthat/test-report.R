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
    skip_if(tryCatch(!file.exists(ovpaged::find_chrome()), error = function(e) TRUE)) ## skip if chrome not available
    ## also temporarily skip on CI (GitHub actions) on Windows
    skip_if(isTRUE(as.logical(Sys.getenv("CI"))) && isTRUE(tolower(Sys.info()[["sysname"]]) %in% "windows"))
    rpt <- vr_match_summary(datavolley::dv_example_file(1), format = "html", remove_nonplaying = FALSE)
    rpt <- vr_match_summary(datavolley::dv_example_file(1), format = "paged_pdf")
    rpt <- vr_match_summary(datavolley::dv_example_file(1), format = "paged_pdf", vote = FALSE)
    rpt <- vr_match_summary(datavolley::dv_example_file(1), format = "paged_pdf", style = "ov1")

    ## beach
    rpt <- vr_match_summary(system.file("extdata/test/&2021ita-w-ross_klineman-pavan_melissa.dvw", package = "volleyreport"), format = "paged_pdf", style = "ov1")
    ## changing icons
    rpt <- vr_match_summary(system.file("extdata/test/&2021ita-w-ross_klineman-pavan_melissa.dvw", package = "volleyreport"), format = "paged_pdf", style = "ov1", plot_icons = FALSE)
    rpt <- vr_match_summary(system.file("extdata/test/&2021ita-w-ross_klineman-pavan_melissa.dvw", package = "volleyreport"), format = "paged_pdf", style = "ov1", plot_icons = TRUE)
    ## change icons around
    pltic <- vr_plot_icons()
    ridx <- sample.int(nrow(pltic))
    pltic[, c("icon_event", "description")] <- pltic[ridx, c("icon_event", "description")]
    rpt <- vr_match_summary(system.file("extdata/test/&2021ita-w-ross_klineman-pavan_melissa.dvw", package = "volleyreport"), format = "paged_pdf", style = "ov1", plot_icons = pltic)
})
