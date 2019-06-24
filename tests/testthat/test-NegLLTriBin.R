context ("Scenario of un wanted inputs")
test_that("NA values are avoided",{
          expect_that(NegLLTriBin(NA,4,0.2),
          throws_error("NA or Infinite or NAN values in the Input"))
        })
test_that("Infinite values are avoided",{
        expect_that(NegLLTriBin(Inf,4,0.2),
        throws_error("NA or Infinite or NAN values in the Input"))
        })
test_that("NAN values are avoided",{
        expect_that(NegLLTriBin(NaN,4,0.3),
        throws_error("NA or Infinite or NAN values in the Input"))
        })

context("Binomial Random variable or frequency issues")
test_that("Negativity Binomial random variable",{
        expect_that(NegLLTriBin(-3,4,0.2),
        throws_error("Binomial random variable or frequency values cannot be negative"))
        })
test_that("Negativity frequency values",{
        expect_that(NegLLTriBin(-3,4,0.2),
        throws_error("Binomial random variable or frequency values cannot be negative"))
        })

context("Mode issues")
test_that("Greater than 1",{
        expect_that(NegLLTriBin(3,4,3),
        throws_error("Mode cannot be less than zero or greater than one"))
        })
test_that("Lesser than 1",{
        expect_that(NegLLTriBin(3,4,-3),
        throws_error("Mode cannot be less than zero or greater than one"))
        })

context("Checking outputs")
test_that("Output value expected",{
        expect_identical(round(NegLLTriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7),4),
                         440.6691)
                         })

test_that("Checking class of output",{
        expect_that(NegLLTriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7),
        is_a("numeric"))
        })
