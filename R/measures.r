#' @title b_01
#' @param pm a list with components a, b, and c
#' @export
B01 <- function(pm) with(pm,{2*(a+b+c)/(2*a+b+c) - 1})

#' @title b_02
#' @param pm a list with components a, b, and c
#' @export
B02 <- function(pm) B01(pm)

#' @title b_03
#' @param pm a list with components a, b, and c
#' @export
B03 <- function(pm) with(pm,{(b+c)/2})

#' @title b_04
#' @param pm a list with components a, b, and c
#' @export
B04 <- function(pm) with(pm,{(b+c)})

#' @title b_05
#' @param pm a list with components a, b, and c
#' @export
B05 <- function(pm) with(pm,{(((a+b+c)^2)/((a+b+c)^2-(2*b*c)))-1})

#' @title b_06
#' @param pm a list with components a, b, and c
#' @export
B06 <- function(pm) with(pm,{log(2*a+b+c)-((2*a*log(2))/(2*a+b+c))-(((a+b)*log(a+b)+(a+c)*log(a+c))/(2*a+b+c))})

#' @title b_07
#' @param pm a list with components a, b, and c
#' @export
B07 <- function(pm) exp(B06(pm))-1
B08 <- function(pm) with(pm,{(b+c)/(2*a+b+c)})
B09 <- function(pm) B08(pm)
B10 <- function(pm) with(pm,{a/(a+b+c)})
B11 <- function(pm) with(pm,{(2*a)/(2*a+b+c)})
B12 <- function(pm) with(pm,{(2*a+b+c)*(1-(a/(a+b+c)))})
B13 <- function(pm) with(pm,{min(b,c)/(max(b,c)+a)})
B14 <- function(pm) with(pm,{1-((a*(2*a+b+c))/(2*(a+b)*(a+c)))})
B15 <- function(pm) with(pm,{(b+c)/(a+b+c)})
B16 <- function(pm) B15(pm)
B17 <- function(pm) with(pm,{min(b,c)/(a+b+c)})
B18 <- function(pm) with(pm,{(b+c)/2})
B19 <- function(pm) with(pm,{(b*c+1)/(((a+b+c)^2-(a+b+c))/2)})
B20 <- function(pm) with(pm,{1-(2*a)/(2*a+b+c)/2})
B21 <- function(pm) with(pm,{a/(a+c)})
B22 <- function(pm) with(pm,{min(b,c)/(min(b,c)+a)})
B23 <- function(pm) with(pm,{(2*aba(b-c))/(2*a+b+c)})
B24 <- function(pm) with(pm,{1-(log((2*a+b+c)/(a+b+c))/log(2))})
