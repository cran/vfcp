#' @export vfpsae
#'
vfpsae = function(rod, xo){
	Lst = list()
	for (i in 1:2){
		if (rod[i] == "weibull") {
			Lst[[i]] = list(scale = xo[2*i - 1], shape = xo[2*i])
		} else {
			if (rod[i] == "gamma") {
			  Lst[[i]] = list(scale = xo[2*i - 1], shape = xo[2*i])
			} else {
				if (rod[i] == "lnorm") {
					Lst[[i]] = list(meanlog = xo[2*i - 1], sdlog = xo[2*i])
				} else {
					if (rod[i] == "norm") {
						Lst[[i]] = list(mean = xo[2*i - 1], sd = xo[2*i])
					} else {
						# betapr | beta
						Lst[[i]] = list(shape1 = xo[2*i - 1], shape2 = xo[2*i])
					}
				}
			}
		}
	}
	# sae = list(list(scale=xo[1], shape=xo[2]),
	#            list(shape1=xo[3], shape2=xo[4]))
  return(list(Lst[[1]], Lst[[2]]))
}
