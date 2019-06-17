context("Scenario of un wanted inputs")
test_that("NA values are avoided",{
          expect_that(fitGammaBin(0:7,c(47,12,43,40,40,41,39,95),NA,3),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("Infinite values are avoided",{
          expect_that(fitGammaBin(0:7,c(47,12,43,40,40,41,39,95),Inf,3),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("NAN values are avoided",{
          expect_that(fitGammaBin(0:7,c(47,54,45,40,40,41,39,95),NaN,5),
          throws_error("NA or Infinite or NAN values in the Input"))
          })

context("Chi-squared issues")
test_that("Chi-squared approximation issues",{
          expect_that(fitGammaBin(0:4,c(20,50,10,4,3),1,6.6),
          shows_message("Chi-squared approximation is not suitable because expected frequency approximates to zero"))
          })
test_that("Chi-squared approximation issues",{
          expect_that(fitGammaBin(0:6,c(2,5,4,40,40,4,3),0.1,2.3),
          shows_message("Chi-squared approximation may be doubtful because expected frequency is less than 5"))
          })

context("Degree of Freedom")
test_that("Degree of freedom less than zero",{
          expect_that(fitGammaBin(c(0,1,2),c(10,11,12),1.71,0.03),
          throws_error("Degrees of freedom cannot be less than or equal to zero"))
          })
test_that("Degree of freedom equal to zero",{
          expect_that(fitGammaBin(c(0,1,2),c(10,11,12),0.1,0.03),
          throws_error("Degrees of freedom cannot be less than or equal to zero"))
          })
