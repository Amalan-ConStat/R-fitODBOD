context("Checking Outputs")
test_that("checking value",{
          expect_identical(round(pGrassiaIIBin(2,4,0.5,0.4),4),
                           0.9391)
         })

test_that("checking class",{
          expect_that(pGrassiaIIBin(2,4,0.5,0.1),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(pGrassiaIIBin(1:2,4,0.5,1)),2)
          })
