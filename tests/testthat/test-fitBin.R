context("Scenario of un wanted inputs")
test_that("NA values are avoided",{
          expect_that(fitBin(0:7,c(47,54,43,40,40,41,39,95),NA),
          throws_error("NA or Infinite or NAN values in the Input"))
        })
test_that("Infinite values are avoided",{
          expect_that(fitBin(0:7,c(47,54,43,40,40,41,39,95),Inf),
          throws_error("NA or Infinite or NAN values in the Input"))
        })
test_that("NAN values are avoided",{
          expect_that(fitBin(0:7,c(47,54,43,40,40,41,39,95),NaN),
          throws_error("NA or Infinite or NAN values in the Input"))
        })

context("Chi-squared issues")
test_that("Chi-squared approximation issues",{
        expect_that(fitBin(1:7,c(147,94,83,40,8,5,1)),
        shows_message("Chi-squared approximation may be doubtful because expected frequency is less than 5"))
        })

context("Degree of Freedom")
test_that("Degree of freedom less than zero",{
        expect_that(fitBin(c(1,2),c(10,12)),
        throws_error("Degrees of freedom cannot be less than or equal to zero"))
        })
test_that("Degree of freedom equal to zero",{
        expect_that(fitBin(c(0,1),c(11,12)),
        throws_error("Degrees of freedom cannot be less than or equal to zero"))
        })
