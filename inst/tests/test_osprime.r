context("Beta OS prime")

test_that("It behaves as expected", {

   data(anemonefish)
   cf <- prepare_networks(anemonefish, TRUE)

   expect_that(beta_os_prime(cf)['Bualo'], equals(0))
   expect_that(beta_os_prime(cf)['Sachiko'] > 0, is_true())

})
