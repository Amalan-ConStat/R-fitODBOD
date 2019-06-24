context("Checking Outputs")
test_that("checking value",{
          expect_identical(round(pCOMPBin(2,4,0.5,0.4),4),
                           0.636)
         })

test_that("checking class",{
          expect_that(pCOMPBin(2,4,0.5,0.1),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(pCOMPBin(1:2,4,0.5,1)),2)
          })
