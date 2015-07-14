context("Betalink function (pairwise)")

test_that("It behaves as expected", {

   data(clownfishes)
   cf <- prepare_networks(clownfishes, TRUE)

   expect_that(betalink(cf[[1]], cf[[1]])$S,  equals(0))
   expect_that(betalink(cf[[2]], cf[[2]])$OS, equals(0))
   expect_that(betalink(cf[[3]], cf[[3]])$WN, equals(0))


   expect_that(betalink(cf[[3]], cf[[4]])$WN > 0, is_true())

})
