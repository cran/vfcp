#' @export vfpsimsi
#' @importFrom copula mvdc rMvdc
#' @importFrom stats spline
#'
vfpsimsi <- function(p, k, mzs, marg, fam, xo, tht, no){
  # if (fam == "gaussian"){
  #   kop = normalCopula(param = tht)
  # } else {
	if (fam == "fgm"){
		kop = fgmCopula(param = tht)
	} else {
		kop = archmCopula(family = fam, param = tht)
  }
	mkop = mvdc(kop, margins = marg, paramMargins = xo)
	simkop = rMvdc(no, mkop)
	#o = order(simkop[, 1])
	# print(simkop)
	gi = which((simkop[, 1] >= mzs$tlc[1]) & (simkop[, 1] <= mzs$brc[1]))
	#print(gi); print(mzs)
	sps = spline(x = mzs$sp[, 1], y = mzs$sp[, 2], xout = simkop[gi, 1], method = "hyman")
	spc = spline(x = mzs$cp[, 1], y = mzs$cp[, 2], xout = simkop[gi, 1], method = "hyman")
	# print(sps); print(spc)
	p[k] = 0
	j = 0
	for(i in gi){
    j = j + 1
	  sm = (simkop[i, 2] <= sps$y[j]) & (simkop[i, 2] >= spc$y[j])
	  # sm = (simkop[i, 2] <= mzs$sp[i, 2]) & (simkop[i, 2] >= mzs$cp[i, 2])
		p[k] = p[k] + ifelse(sm, 1, 0)
	}
		p[k] = p[k]/no
	print(paste(k, no, "sim. ... p =", p[k]))
	return(p[k])
}
