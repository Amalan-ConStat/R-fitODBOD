estimate <- EstMLETriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)
fitTB<-fitTriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,estimate$mode)

context("Checking outputs")
test_that("Output value expected",{
          expect_identical(round(Overdispersion(fitTB),4),
                           0.1796)
                           })

test_that("Checking class of output",{
          expect_that(Overdispersion(fitTB),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(Overdispersion(fitTB)),1)
          })

estimate <- EstMGFBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)
fitBB<-fitBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,estimate$a,estimate$b)

context("Checking outputs")
test_that("Output value expected",{
          expect_identical(round(Overdispersion(fitBB),4),
                           0.086)
                           })

test_that("Checking class of output",{
          expect_that(Overdispersion(fitBB),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(Overdispersion(fitBB)),1)
          })

estimate <- EstMLEGammaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.5,0.7)
fitGaB<-fitGammaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,estimate@coef[1],estimate@coef[2])

context("Checking outputs")
test_that("Checking class of output",{
          expect_that(Overdispersion(fitGaB),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(Overdispersion(fitGaB)),1)
          })

estimate <- EstMLEGrassiaIIBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.5,0.7)
fitGraB<-fitGrassiaIIBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,estimate@coef[1],estimate@coef[2])

context("Checking outputs")
test_that("Checking class of output",{
          expect_that(Overdispersion(fitGraB),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(Overdispersion(fitGraB)),1)
          })

estimate <- EstMLEGHGBB(Alcohol_data$Days,Alcohol_data$week1,1,7,10)
fitGHB<-fitGHGBB(Alcohol_data$Days,Alcohol_data$week1,estimate@coef[1],estimate@coef[2],
                        estimate@coef[3])

context("Checking outputs")
test_that("Output value expected",{
          expect_identical(round(Overdispersion(fitGHB),4),
                           0.4325)
                           })

test_that("Checking class of output",{
          expect_that(Overdispersion(fitGHB),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(Overdispersion(fitGHB)),1)
          })

estimate <- EstMLEMcGBB(Alcohol_data$Days,Alcohol_data$week1,1,7,10)
fitMcB <- fitMcGBB(Alcohol_data$Days,Alcohol_data$week1,estimate@coef[1],estimate@coef[2],
                   estimate@coef[3])

context("Checking outputs")
test_that("Checking class of output",{
          expect_that(Overdispersion(fitMcB),
          is_a("numeric"))
          })

test_that("checking length of output",{
          expect_equal(length(Overdispersion(fitMcB)),1)
          })
