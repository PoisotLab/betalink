context("Network beta-diversity function")

test_that("the complete argument works", {

  data(anemonefish)
  cf <- prepare_networks(anemonefish, TRUE)
  cf3 <- cf[c(1,2,3)]

  expect_that(nrow(network_betadiversity(cf3, complete=TRUE)),  equals(6))
  expect_that(nrow(network_betadiversity(cf3, complete=FALSE)),  equals(3))

})

test_that("lists of two networks are OK", {

  data(anemonefish)
  cf <- prepare_networks(anemonefish, TRUE)
  cf2 <- cf[c(1,2)]

  expect_that(nrow(network_betadiversity(cf2, complete=TRUE)),  equals(2))
  expect_that(nrow(network_betadiversity(cf2)),  equals(1))

})
