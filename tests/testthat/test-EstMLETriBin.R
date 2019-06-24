context("Scenario of un wanted inputs")
test_that("NA values are avoided",{
          expect_that(EstMLETriBin(NA,4),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("NAN values are avoided",{
          expect_that(EstMLETriBin(NaN,4),
          throws_error("NA or Infinite or NAN values in the Input"))
          })
test_that("Infinite values are avoided",{
          expect_that(EstMLETriBin(Inf,4),
          throws_error("NA or Infinite or NAN values in the Input"))
          })

context("Checking outputs")
test_that("estimate parameter mode",{
          expect_identical(round(EstMLETriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)$mode,6),
                           0.707276)
                           })

test_that("minimized negative ll value",{
          expect_identical(round(EstMLETriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)$min,4),
                           440.6583)
                           })

test_that("Checking class of output",{
          expect_that(EstMLETriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre),
          is_a("mlTRI"))
          })

test_that("Checking class of output",{
          expect_that(EstMLETriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre),
          is_a("ml"))
          })
