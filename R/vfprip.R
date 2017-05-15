#' @export vfprip
#'
vfprip <- function(Ck, hom, jm){
	rz = hom - Ck
	bz = rz/jm
	vb = bz/50
	va = vb/10
	return(Ck + c(1e-14, 1e-09, seq(va, vb, va), seq(vb + vb, bz, vb),
	              seq(bz + bz, hom - Ck, bz)))
}
