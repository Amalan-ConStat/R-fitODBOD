context ("Scenario of un wanted inputs")
test_that("NA values are avoided",{
          expect_that(NegLLGammaBin(NA,4,0.1,3),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("Infinite values are avoided",{
          expect_that(NegLLGammaBin(Inf,4,0.1,3),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("NAN values are avoided",{
          expect_that(NegLLGammaBin(NaN,4,0.1,3),
          throws_error("NA or Infinite or NAN values in the Input"))
          })

context("Shape parameter issues")
test_that("shape parameter a",{
          expect_that(NegLLGammaBin(2,4,-3,3),
          throws_error("Shape parameters cannot be less than or equal to zero"))
          })
test_that("shape parameter b",{
          expect_that(NegLLGammaBin(2,4,1,-3),
          throws_error("Shape parameters cannot be less than or equal to zero"))
          })

context("Binomial Random variable or frequency issues")
test_that("Negativity Binomial random variable",{
          expect_that(NegLLGammaBin(-3,4,0.2,4),
          throws_error("Binomial random variable or frequency values cannot be negative"))
          })
test_that("Negativity Binomial random variable",{
          expect_that(NegLLGammaBin(3,-4,0.2,4),
          throws_error("Binomial random variable or frequency values cannot be negative"))
          })

context("Checking outputs")
test_that("Output value expected",{
          expect_identical(round(NegLLGammaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,1),4),
                           500.6763)
                           })

test_that("Checking class of output",{
          expect_that(NegLLGammaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,1),
          is_a("numeric"))
          })
