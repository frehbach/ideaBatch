context("Testing Main Functions")

test_that("Tutorials work correctly", {
    currentlyExistingTutorials <- c(1)

    expect_equal(length(ideaTutorial()), length(currentlyExistingTutorials))
    for(e in ideaTutorial()){
        expect_true(checkmate::check_string(e))
    }

    expect_error(ideaTutorial(-1), regexp = "tutorial")
    expect_error(ideaTutorial(0), regexp = "tutorial")
    expect_error(ideaTutorial(7), regexp = "tutorial")
    expect_error(ideaTutorial(length(currentlyExistingTutorials)+1), regexp = "tutorial")
    expect_error(ideaTutorial(NA), regexp = "Index was not numeric")
    expect_error(ideaTutorial("Hi"), regexp = "Index was not numeric")
})
