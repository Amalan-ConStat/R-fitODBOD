context("Checking Outputs")
test_that("checking value",{
          expect_identical(round(pGHGBB(2,4,0.5,0.4,1),4),
                           0.5056)
         })

test_that("checking class",{
          expect_that(pGHGBB(2,4,0.5,0.1,1),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(pGHGBB(1:2,4,0.5,0.4,1)),2)
          })
