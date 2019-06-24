context("Checking the Epidemic Cold")
test_that("Comparing values of Cases",{
        expect_equal(Epidemic_Cold$Cases,seq(0,4))
        })

test_that("Comparing values of Frequency Family",{
        expect_equal(Epidemic_Cold$Families,c(423,199,39,3,0))
        })

test_that("Comparing values of Frequency Father",{
        expect_equal(Epidemic_Cold$Father,c(53,31,4,0,0))
        })

test_that("Comparing values of Frequency Mother",{
        expect_equal(Epidemic_Cold$Mother,c(75,25,4,1,0))
        })

test_that("Comparing values of Frequency SChlid",{
        expect_equal(Epidemic_Cold$SChild,c(148,77,22,2,0))
        })

test_that("Comparing values of Frequency PSChild",{
        expect_equal(Epidemic_Cold$PSChild,c(147,66,9,0,0))
        })
