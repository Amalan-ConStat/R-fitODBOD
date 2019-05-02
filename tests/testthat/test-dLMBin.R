context ("Scenario of un wanted inputs")
test_that("NA values are avoided",{
  expect_that(dLMBin(NA,4,0.1,.3),
              throws_error("NA or Infinite or NAN values in the Input"))
              })
test_that("Infinite values are avoided",{
  expect_that(dLMBin(Inf,4,0.1,.3),
              throws_error("NA or Infinite or NAN values in the Input"))
              })
test_that("NAN values are avoided",{
  expect_that(dLMBin(NaN,4,0.1,.3),
              throws_error("NA or Infinite or NAN values in the Input"))
              })
