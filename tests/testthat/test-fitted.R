estimate <- EstMGFBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)
fitBB<-fitBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,estimate$a,estimate$b)

context("Checking Outputs")
test_that("checking value 1",{
          expect_identical(round(fitted(fitBB))[1],
                           34)
                           })

test_that("checking value 2",{
          expect_identical(round(fitted(fitBB))[2],
                           97)
                           })

test_that("checking class",{
          expect_that(fitted(fitBB),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(fitted(fitBB)),4)
          })
