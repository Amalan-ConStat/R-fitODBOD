context("Scenario of un wanted inputs")
test_that("NA values are avoided",{
          expect_that(EstMGFBetaBin(0:7,c(47,54,43,40,40,41,NA,95)),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("Infinite values are avoided",{
          expect_that(EstMGFBetaBin(0:7,c(47,54,43,40,40,Inf,39,95)),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("NAN values are avoided",{
          expect_that(EstMGFBetaBin(0:7,c(47,54,43,40,40,NaN,39,95)),
          throws_error("NA or Infinite or NAN values in the Input"))
          })

context("Checking outputs")
test_that("estimate parameter a",{
          expect_identical(round(EstMGFBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)$a,6),
                           6.167982)
                           })

test_that("estimate parameter b",{
          expect_identical(round(EstMGFBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)$b,6),
                           4.455237)
                           })

test_that("minimized negative ll value",{
          expect_identical(round(EstMGFBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)$min,4),
                          436.8131)
                          })

test_that("Checking class of output",{
          expect_that(EstMGFBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre),
          is_a("mgf"))
          })

