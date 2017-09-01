#' @export vfpint
#' @importFrom polyCub xylist
#' @importFrom cubature hcubature
#'
vfpint <- function(p, k, mzu, marg, fam, xo, tht){
	fca <- function(x){
		return(fxa(x, marg, cofam = fam, xo, tht))
	}

	plgo = list(mzu$sps, mzu$spc[length(mzu$spc$x):1])
	# print(paste("k =", k, "plgo")); print(plgo)
	goxy = xylist(plgo, reverse = FALSE)
	# print("goxy"); print(goxy)
	p[k] = 0
	for(i in 2:(length(goxy[[1]]$x))){
		rb = c(goxy[[1]]$x[i], goxy[[1]]$y[i])
		lb = c(goxy[[2]]$x[i-1], goxy[[2]]$y[i-1])
		p[k] = p[k] + hcubature(fca, lb, rb)$integral
	}
	print(paste(k, "hcubature ... p =", p[k]))
	return(p[k])
}
