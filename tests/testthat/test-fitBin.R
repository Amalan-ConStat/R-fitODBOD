context("Scenario of un wanted inputs")
test_that("NA values are avoided",{
          expect_that(fitBin(0:7,c(47,54,43,40,40,41,39,95),NA,F),
          throws_error("NA or Infinite or NAN values in the Input"))
        })
test_that("Infinite values are avoided",{
          expect_that(fitBin(0:7,c(47,54,43,40,40,41,39,95),Inf,F),
          throws_error("NA or Infinite or NAN values in the Input"))
        })
test_that("NAN values are avoided",{
          expect_that(fitBin(0:7,c(47,54,43,40,40,41,39,95),NaN,F),
          throws_error("NA or Infinite or NAN values in the Input"))
        })

context("Chi-squared issues")
test_that("Chi-squared approximation issues",{
        expect_that(fitBin(0:7,c(47,54,43,40,40,41,39,95),0.003),
        gives_warning("Chi-squared approximation is not suitable because expected frequency approximates to zero"))
        })

