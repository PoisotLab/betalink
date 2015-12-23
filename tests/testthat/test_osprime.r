context("Beta OS prime")

test_that("gives the correct values on the anemonefish example", {

   data(anemonefish)
   cf <- prepare_networks(anemonefish, TRUE)

   expect_that(beta_os_prime(cf)['Bualo'] < 1, is_true())
   expect_that(beta_os_prime(cf)['Sachiko'] > 0, is_true())

})
