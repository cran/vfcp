#' @export vfmrg
#'
vfmrg <- function(rdj, i, cosi, yo, cdf)
{
	if (!cdf) posi = 1 - cosi else posi = cosi
	if (rdj[i] == "weibull"){
		ha = qweibull(posi, scale = yo[2*i - 1], shape = yo[2*i])
	} else {
		if (rdj[i] == "gamma"){
			ha = qgamma(posi, scale = yo[2*i - 1], shape = yo[2*i])
		} else {
			if (rdj[i] == "lnorm"){
				ha = qlnorm(posi, meanlog = yo[2*i - 1], sdlog = yo[2*i])
			} else {
				ha = qnorm(posi, mean = yo[2*i - 1], sd = yo[2*i])
			}
		}
	}
	return(ha)
}