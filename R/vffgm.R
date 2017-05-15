#' @export vffgm
#'
vffgm <- function(C, u, tht){
	a = -(1 - u)*tht
	b = 1 + (1 - u)*tht
	c = -C/u
	return((-b + sqrt(b*b - 4*a*c))/(2*a))
}
