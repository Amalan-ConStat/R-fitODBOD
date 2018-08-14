context("Scenario of un wanted inputs")
test_that("NA values are avoided",{
          expect_that(fitMultiBin(0:7,c(47,54,43,40,40,41,39,95),NA,0.003),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("Infinite values are avoided",{
          expect_that(fitMultiBin(0:7,c(47,54,43,40,40,41,39,95),Inf,0.003),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("NAN values are avoided",{
          expect_that(fitMultiBin(0:7,c(47,54,43,40,40,41,39,95),NaN,0.003),
          throws_error("NA or Infinite or NAN values in the Input"))
          })

context("Chi-squared issues")
test_that("Chi-squared approximation issues",{
          expect_that(fitMultiBin(0:7,c(47,54,43,40,40,41,39,95),0.1,0.003),
          gives_warning("Chi-squared approximation is not suitable because expected frequency approximates to zero"))
          })
test_that("Chi-squared approximation issues",{
          expect_that(fitMultiBin(0:3,c(43,40,40,41),0.54,15.0003),
          gives_warning("Chi-squared approximation may be doubtful because expected frequency is less than 5"))
          })
