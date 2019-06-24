context("Checking Outputs")
test_that("checking value",{
          expect_identical(round(pUniBin(2,4),1),
                           0.6)
         })

test_that("checking class",{
          expect_that(pUniBin(2,4),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(pUniBin(1:2,4)),2)
          })
