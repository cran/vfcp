#' @export CSmeze
#' @importFrom stats spline
#'
CSmeze <- function(cx, pro, xo, marg, cofam, tht){
	u = vfpripo(cx[1], pro)
	# u = vfprifo(cx, pro)
	v = vfex(C = cx, u, th = tht, fm = cofam)
	c1 = vfmrg(marg, 1, u, yo = xo, cdf = TRUE)
	s1 = vfmrg(marg, 1, u, yo = xo, cdf = FALSE)
	c2 = vfmrg(marg, 2, v, yo = xo, cdf = TRUE)
	s2 = vfmrg(marg, 2, v, yo = xo, cdf = FALSE)
	asx = data.frame(s1 = s1, s2 = s2)
	acx = data.frame(c1 = c1, c2 = c2)
	inx = max(which(asx$s1 <= min(acx$c1))) # x1, x2 and y1, y2 for tlc
	xnx = max(which(acx$c1 <= max(asx$s1))) # x1, x2 and y1, y2 for brc
	iny = max(which((acx$c2) >= max(asx$s2))) # x4, x3 and y4, y3 for tlc
	xny = min(which((asx$s2) >= min(acx$c2))) # x3, x4 and y3, y4 for brc

	#== top left corner =====================
	ixt = c(inx, inx - 1, iny+1, iny)
	xt = c(asx$s1[ixt[1:2]], acx$c1[ixt[3:4]])
	yt = c(asx$s2[ixt[1:2]], acx$c2[ixt[3:4]])
	tlc = prunikus(xt, yt)
	# print(tlc)
	#== bottom right corner ========================
	iyt = c(xnx, xnx + 1, xny-1, xny)
	xb = c(acx$c1[iyt[1:2]], asx$s1[iyt[3:4]])
	yb = c(acx$c2[iyt[1:2]], asx$s2[iyt[3:4]])
	brc = prunikus(xb, yb)
	# print(brc)
	#====================
	pasx = rbind(tlc, asx, brc)
	susx = subset(pasx, pasx$s1 >= tlc[1] & pasx$s1 <= brc[1])
	os = order(susx$s1)

	pacx = rbind(tlc, acx, brc)
	sucx = subset(pacx, pacx$c1 >= tlc[1] & pacx$c1 <= brc[1])
	oc = order(sucx$c1)
	###########################
	nxw = length(sucx$c1) * 4
	sps = spline(x = susx[os, 1], y = susx[os, 2], n = nxw, method = "natural")
	spc = spline(x = sucx[oc, 1], y = sucx[oc, 2], n = nxw, method = "natural")
	return(list(tlc = tlc, brc = brc, sps = sps, spc = spc))
}
