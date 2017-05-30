#' @export vfploto
#' @importFrom graphics plot lines grid legend
#'
vfploto <- function(cx, pro, fam, marg, xo, tht,
									 cdf=TRUE, plt=TRUE, rtn=FALSE)
{
	if (!plt & !rtn) stop(paste("plt =", plt, "and", "rtn =", rtn))
	ncx = length(cx)
	tp = ifelse(cdf == TRUE, "CDF", "Survival")
	if (any(pro[2:length(pro)] < 1) & any(pro >= 1)) stop("error: wrong 'pro'")
	if (pro[1] >= 1) stop("pro[1] >= 1")
	if (all(pro < 1)) pda = FALSE else pda = TRUE
	cz = list()
	if (plt | rtn){
		if (plt){
			if (pda){
				up = vfpripo(cx[1], pro)
				uk = vfpripo(cx[ncx], pro)
			} else {
				up = vfprifo(cx[1], pro)
				uk = vfprifo(cx[ncx], pro)
			}
			# if (!cdf){up = 1 -up; uk = 1 - uk}
			vp = vfex(cx[1], up, th = tht, fm = fam)
			vk = vfex(cx[ncx], uk, th = tht, fm = fam)
			xyxk = vfmrg(rdj = marg, i = 1, cosi = uk, yo = xo, cdf)
			xyxp = vfmrg(rdj = marg, i = 1, cosi = up, yo = xo, cdf)
			xyyk = vfmrg(rdj = marg, i = 2, cosi = vk, yo = xo, cdf)
			xyyp = vfmrg(rdj = marg, i = 2, cosi = vp, yo = xo, cdf)
			if (cdf){
				limx = c(min(0, xyxp), max(0, xyxk))
				limy = c(min(0, xyyp), max(0, xyyk))
			} else {
				limx = c(min(0, xyxk), max(0, xyxp))
				limy = c(min(0, xyyk), max(0, xyyp))
			}

			plot(x=NULL, y=NULL, xlim=limx, ylim=limy,
					 xlab = paste(marg[1], " (", xo[1], ", ", xo[2], ")", sep = ""),
					 ylab = paste(marg[2], " (", xo[3], ", ", xo[4], ")", sep = ""),
					 main = paste(tp, fam, "tht =", tht))
			legend("topright", legend = cx, text.col = c(1:length(cx)), bty = "n")
			grid(col = 2)
		}
		if (pda) slp = length(vfpripo(cx[1], pro)) else slp = length(vfprifo(cx[1], pro))
		u = array(NA, c(ncx, slp))
		v = array(NA, c(ncx, slp))
		for (k in 1:ncx){
			if (pda) u[k, ] = vfpripo(cx[k], pro) else u[k, ] = vfprifo(cx[1], pro)
			v[k, ] = vfex(cx[k], u[k, ], th = tht, fm = fam)
			xyx = vfmrg(rdj = marg, i = 1, cosi = u[k, ], yo = xo, cdf)
			xyy = vfmrg(rdj = marg, i = 2, cosi = v[k, ], yo = xo, cdf)
			if (plt){
				lines(xyx, xyy, col = k, type = "l")
			}
			if (rtn){
				cz[[k]] = list(Type = tp, P = cx[k], x = xyx, y = xyy,
											 u = u[k, ], v = v[k, ])
			}
		}
	}
	return(cz)
}
