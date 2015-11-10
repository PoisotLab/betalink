#' @title Whittaker
#' @param pm a list with components a, b, and c
#' @export
B01 <- function(pm) with(pm,{2*(a+b+c)/(2*a+b+c) - 1})

#' @title Harrison
#' @param pm a list with components a, b, and c
#' @export
B02 <- function(pm) B01(pm)

#' @title Cody
#' @param pm a list with components a, b, and c
#' @export
B03 <- function(pm) with(pm,{(b+c)/2})

#' @title WeiherBoylen
#' @param pm a list with components a, b, and c
#' @export
B04 <- function(pm) with(pm,{(b+c)})

#' @title Routledge
#' @param pm a list with components a, b, and c
#' @export
B05 <- function(pm) with(pm,{(((a+b+c)^2)/((a+b+c)^2-(2*b*c)))-1})

#' @title WilsonShmida
#' @param pm a list with components a, b, and c
#' @export
B06 <- function(pm) with(pm,{log(2*a+b+c)-((2*a*log(2))/(2*a+b+c))-(((a+b)*log(a+b)+(a+c)*log(a+c))/(2*a+b+c))})

#' @title Routledge2
#' @param pm a list with components a, b, and c
#' @export
B07 <- function(pm) exp(B06(pm))-1

#' @title WilsonShmida2
#' @param pm a list with components a, b, and c
#' @export
B08 <- function(pm) with(pm,{(b+c)/(2*a+b+c)})

#' @title MourelleEzcurra
#' @param pm a list with components a, b, and c
#' @export
B09 <- function(pm) B08(pm)

#' @title Jaccard
#' @param pm a list with components a, b, and c
#' @export
B10 <- function(pm) with(pm,{a/(a+b+c)})

#' @title Sorensen
#' @param pm a list with components a, b, and c
#' @export
B11 <- function(pm) with(pm,{(2*a)/(2*a+b+c)})

#' @title Magurran
#' @param pm a list with components a, b, and c
#' @export
B12 <- function(pm) with(pm,{(2*a+b+c)*(1-(a/(a+b+c)))})

#' @title Harrison2
#' @param pm a list with components a, b, and c
#' @export
B13 <- function(pm) with(pm,{min(b,c)/(max(b,c)+a)})

#' @title Cody2
#' @param pm a list with components a, b, and c
#' @export
B14 <- function(pm) with(pm,{1-((a*(2*a+b+c))/(2*(a+b)*(a+c)))})

#' @title ColwellCoddington
#' @param pm a list with components a, b, and c
#' @export
B15 <- function(pm) with(pm,{(b+c)/(a+b+c)})

#' @title Gaston
#' @param pm a list with components a, b, and c
#' @export
B16 <- function(pm) B15(pm)

#' @title Williams
#' @param pm a list with components a, b, and c
#' @export
B17 <- function(pm) with(pm,{min(b,c)/(a+b+c)})

#' @title Lande
#' @param pm a list with components a, b, and c
#' @export
B18 <- function(pm) with(pm,{(b+c)/2})

#' @title Williams2
#' @param pm a list with components a, b, and c
#' @export
B19 <- function(pm) with(pm,{(b*c+1)/(((a+b+c)^2-(a+b+c))/2)})

#' @title HarteKinzig
#' @param pm a list with components a, b, and c
#' @export
B20 <- function(pm) with(pm,{1-(2*a)/(2*a+b+c)/2})

#' @title Ruggiero
#' @param pm a list with components a, b, and c
#' @export
B21 <- function(pm) with(pm,{a/(a+c)})

#' @title Lennon
#' @param pm a list with components a, b, and c
#' @export
B22 <- function(pm) with(pm,{min(b,c)/(min(b,c)+a)})

#' @title Lennon2
#' @param pm a list with components a, b, and c
#' @export
B23 <- function(pm) with(pm,{(2*aba(b-c))/(2*a+b+c)})

#' @title b_07
#' @param pm a list with components a, b, and c
#' @export
B24 <- function(pm) with(pm,{1-(log((2*a+b+c)/(a+b+c))/log(2))})
