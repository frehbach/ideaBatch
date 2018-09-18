testthat::skip_on_travis()
testthat::skip_on_appveyor()

context("Testing Tutorials")

test_that("Tutorials work",{
    tut1 <- system.file("Tutorials/Tutorial_1.Rmd",package = "ideaBatch")
    knitr::purl(tut1, "deleteMe.R", documentation = 2)
    expect_error(expect_warning(source("deleteMe.R")),regexp = NA)
    unlink("deleteMe.R")
})
