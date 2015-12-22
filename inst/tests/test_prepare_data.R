context("Preparing data for analysis")

test_that("It behaves as expected", {
   data(anemonefish)
   cf <- prepare_networks(anemonefish, TRUE)
   expect_that(typeof(cf$Fukui), equals("list"))
})

test_that("It works with no names", {
   data(anemonefish)
   x <- unname(anemonefish)
   cf <- prepare_networks(x, TRUE)
   expect_that(typeof(cf$Fukui), equals("list"))
})
