context("Checking Outputs")
test_that("checking value",{
          expect_identical(round(pAddBin(2,4,0.5,0.4),4),
                           0.5375)
         })

test_that("checking class",{
          expect_that(pAddBin(2,4,0.5,0.1),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(pAddBin(1:2,4,0.5,0.1)),2)
          })
