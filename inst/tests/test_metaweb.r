context("Beta OS prime")

test_that("It behaves as expected", {

   data(anemonefish)
   cf <- prepare_networks(anemonefish, TRUE)

   # If there is a single element, the metaweb function returns
   # this element
   expect_that(metaweb(cf[[1]]), equals(cf[[1]]))

})
