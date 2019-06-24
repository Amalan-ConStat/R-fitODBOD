context("Checking Outputs")
test_that("checking value",{
          expect_identical(round(pTriBin(2,4,0.5),4),
                           0.6375)
         })

test_that("checking class",{
          expect_that(pTriBin(2,4,0.5),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(pTriBin(1:2,4,0.5)),2)
          })
