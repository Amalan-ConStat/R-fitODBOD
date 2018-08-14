context("Scenario of un wanted inputs")
test_that("NA values are avoided",{
          expect_that(fitKumBin(0:7,c(47,12,43,40,40,41,39,95),NA,3,1000),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("Infinite values are avoided",{
          expect_that(fitKumBin(0:7,c(47,12,43,40,40,41,39,95),Inf,3,1000),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("NAN values are avoided",{
          expect_that(fitKumBin(0:7,c(47,54,45,40,40,41,39,95),NaN,5,1000),
          throws_error("NA or Infinite or NAN values in the Input"))
          })

context("Chi-squared issues")
test_that("Chi-squared approximation issues",{
          expect_that(fitKumBin(0:6,c(2,5,4,40,40,4,3),0.1,3,7000),
          gives_warning("Chi-squared approximation is not suitable because expected frequency approximates to zero"))
          })
test_that("Chi-squared approximation issues",{
          expect_that(fitKumBin(0:4,c(2,5,1,4,3),1.1,1.0,10000),
          gives_warning("Chi-squared approximation may be doubtful because expected frequency is less than 5"))
          })
