estimate <- EstMGFBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)

context("Checking outputs")
test_that("Output value expected",{
          expect_identical(round(AIC(estimate),4),
                           877.6263)
                           })

test_that("Checking class of output",{
          expect_that(AIC(estimate),
          is_a("numeric"))
          })

fitBB<-fitBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,estimate$a,estimate$b)

context("Checking outputs")
test_that("Output value expected",{
          expect_identical(round(AIC(fitBB),4),
                           440.8131)
})

test_that("Checking class of output",{
          expect_that(AIC(fitBB),
          is_a("numeric"))
          })

