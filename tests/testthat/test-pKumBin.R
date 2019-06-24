context("Checking Outputs")
test_that("checking value",{
          expect_identical(round(pKumBin(1,8,1.05,1.04),4),
                           0.2149)
         })

test_that("checking class",{
          expect_that(pBetaBin(1,8,1.05,1.04),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(pBetaBin(1:2,8,1.05,1.04)),2)
          })
