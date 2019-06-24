context("Scenario of un wanted inputs")
test_that("NA values are avoided",{
          expect_that(EstMLEAddBin(NA,4),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("NAN values are avoided",{
          expect_that(EstMLEAddBin(NaN,4),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("Infinite values are avoided",{
          expect_that(EstMLEAddBin(Inf,4),
          throws_error("NA or Infinite or NAN values in the Input"))
          })

context("Checking outputs")
test_that("estimate parameter p",{
          expect_identical(round(EstMLEAddBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)$p,6),
                          0.580941)
                          })

test_that("estimate parameter alpha",{
          expect_identical(round(EstMLEAddBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)$alpha,6),
                          0.088305)
                          })

test_that("minimized negative ll value",{
          expect_identical(round(EstMLEAddBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)$min,4),
                           436.7429)
                           })

test_that("Checking class of output",{
          expect_that(EstMLEAddBin(Chromosome_data$No.of.Asso,Chromosome_data$fre),
          is_a("mlAB"))
          })
