#' @export vfplot
#' @importFrom graphics plot lines grid legend
#'
vfplot <- function(cx, hor, dol, fam, marg, xo, tht, cdf=TRUE,
									 plt=TRUE, rtn=FALSE)
{
	if (!plt & !rtn) stop(paste("plt =", plt, "and", "rtn =", rtn))
	ncx = length(cx)
	tp = ifelse(cdf == TRUE, "CDF", "Survival")
	cz = list()
	if (plt | rtn){
		if (plt){
			xyh = vfmargi(cx, ncx, hor, dol, fam, marg, cdf, xo, tht)
			xyd = vfmargi(cx, 1, hor, dol, fam, marg, cdf, xo, tht)
			limx = c(0, max(xyh$gweo, xyd$gweo))
			limy = c(0, max(xyh$glno, xyd$glno))
			plot(x=NULL, y=NULL, xlim=limx, ylim=limy,
					 xlab = paste(marg[1], " (", xo[1], ", ", xo[2], ")", sep = ""),
					 ylab = paste(marg[2], " (", xo[3], ", ", xo[4], ")", sep = ""),
					 main = paste(tp, fam, "tht =", tht))
			legend("topright", legend = cx, text.col = c(1:length(cx)), bty = "n")
			grid(col = 2)
		}
		for (k in 1:ncx){
			xy = vfmargi(cx, k, hor, dol, fam, marg, cdf, xo, tht)
			if (plt){
				if (cdf){
					lines(c(xy$gweo[1], xy$gweo), c(limy[2], xy$glno), col = k, type = "l")
				} else {
					lines(c(xy$gweo[1], xy$gweo), c(limy[1], xy$glno), col = k, type = "l")
				}
			}
			if (rtn){
				cz[[k]] = list(Type = tp, P = cx[k], x = xy$gweo, y = xy$glno,
											 u = xy$u, v = xy$v)
			}
		}
	}
	return(cz)
}
