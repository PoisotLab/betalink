#' @title Measure the distance between a network and its metaweb
#' @description
#' Returns the values of beta OS', i.e. the distace between all realizations, and the revelant subset from the metaweb
#'
#' @param W a list of networks
#' @param ... additional arguments to be passed to \link{betalink}
#'
#' @return An array of the values of Beta OS'
#'
#' @export
beta.os_prime <- function(W, ...){
	metaweb <- metaweb(W)$web
	os_prime <- NULL
	for(w in W){
		partitions <- betalink(w,metaweb[rownames(w),colnames(w)],...)
		os_prime <- c(os_prime,partitions$OS)
	}
	return(os_prime)
}
