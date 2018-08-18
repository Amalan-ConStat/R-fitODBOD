context("Checking the Alcohol Data")
test_that("Comparing length of Days",{
        expect_equal(length(Alcohol_data$Days),seq(0,7))
        })
