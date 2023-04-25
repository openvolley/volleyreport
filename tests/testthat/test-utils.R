context("internal utilities")

test_that("various utils work", {
    expect_null(prc(c()))
    expect_equal(prc(1), "1%")
    expect_true(is.na(prc(NA)))
    expect_equal(prc(c(1, NA)), c("1%", NA_character_))
    expect_equal(prc(c(1, NA), before = "[", after = "%)"), c("[1%)", NA_character_))
})

test_that("internal functions are ok", {
    icns <- volleyreport:::vr_plot_icons()
    expect_false(any(duplicated(icns$icon_name)))
    expect_false(any(duplicated(icns$icon_event)))
})
