context("Checking Outputs")
test_that("checking value",{
          expect_identical(round(pCorrBin(2,4,0.5,0.004),4),
                           0.6815)
         })

test_that("checking class",{
          expect_that(pCorrBin(2,4,0.5,0.0001),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(pCorrBin(1:2,4,0.5,0.0001)),2)
          })
