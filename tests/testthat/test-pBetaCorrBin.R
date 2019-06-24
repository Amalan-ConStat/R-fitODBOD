context("Checking Outputs")
test_that("checking value",{
          expect_identical(round(pBetaCorrBin(2,8,0.0005,0.4,1.1),4),
                           0.6501)
         })

test_that("checking class",{
          expect_that(pBetaCorrBin(2,8,0.0005,0.4,1.1),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(pBetaCorrBin(1:2,8,0.0005,0.4,1.1)),2)
          })
