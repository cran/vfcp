#' @export vfalihaq
#'
vfalihaq <- function(C, u, tht){
  # Ali–Mikhail–Haq
	return(C*(1 - tht*(1 - u))/(u - tht*C*(1 - u)))
}

#' @export vfjoe
#'
vfjoe <- function(C, u, tht){
  # tht >= 1
  return(1 - (((1 - C)^tht - (1 - u)^tht)/(1 - (1 - u)^tht))^(1/tht))
}
