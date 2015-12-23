context("Metaweb")

test_that("it works on a single network", {

   data(anemonefish)
   cf <- prepare_networks(anemonefish, TRUE)
   expect_equal(length(metaweb(list(n = cf[[1]]))), 1)

})

test_that("the good exceptions are raised", {

   data(anemonefish)
   cf <- prepare_networks(anemonefish, TRUE)

   expect_that(metaweb(list(cf[[1]])), gives_warning(regexp="name"))
   expect_that(metaweb(list(n = cf[[1]])), gives_warning(regexp="single"))

})
