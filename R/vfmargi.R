#' @export vfmargi
#' @importFrom stats qweibull qgamma qlnorm qnorm
#'
vfmargi <- function(cx, k, hor, dol, fam, marg, cdf, xo, tht){
	u = vfprip(Ck = cx[k], hom = hor, jm = dol)
	if (fam == "clayton"){
		gu = vfclayton(cx[k], u, tht)
	} else {
		if (fam == "frank"){
			gu = vffrank(cx[k], u, tht)
		} else {
			if (fam == "gumbel"){
				gu = vfgumbel(cx[k], u, tht)
			} else {
				if (fam == "alihaq"){
					gu = vfalihaq(cx[k], u, tht)
				} else {
					if (fam == "joe"){
					 gu = vfjoe(cx[k], u, tht)
					} else {
						gu = vffgm(cx[k], u, tht)
					}
				}
			}
		}
	}
  # print(xo)
  if (cdf == FALSE){u = 1 - u; gu = 1 - gu}
	if (marg[1] == "weibull"){
		gweo = qweibull(u, shape = xo[2], scale = xo[1])
	} else {
	if (marg[1] == "gamma"){
		gweo = qgamma(u, shape = xo[2], scale = xo[1])
	} else {
		if (marg[1] == "lnorm"){
			gweo = qlnorm(u, meanlog = xo[1], sdlog = xo[2])
		} else {
			gweo = qnorm(u, mean = xo[1], sd = xo[2])
		}
	}
}
if (marg[2] == "weibull"){
	glno = qweibull(gu, shape = xo[4], scale = xo[3])
} else {
	if (marg[2] == "gamma"){
		glno = qgamma(gu, shape = xo[4], scale = xo[3])
	} else {
		if (marg[2] == "lnorm"){
			glno = qlnorm(gu, meanlog = xo[3], sdlog = xo[4])
		} else {
			glno = qnorm(gu, mean = xo[3], sd = xo[4])
		}
	}
}

return(list(gweo = gweo, glno = glno, u = u, v = gu))
}
