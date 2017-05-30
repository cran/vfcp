#' @export vfpripo
#'
vfpripo <- function(ck, pro)
{
	npro = length(pro)
	b = numeric(1)
	b[1] = pro[1] - ck
	for(i in 2:npro)
	{
		b[i] = b[i - 1]/pro[i]
	}
	bb = 0
	a = list()
	for(j in npro:2)
	{
		if (j == npro) a[[j]] = seq(b[j], b[j - 1], b[j]) else {
			a[[j]] = seq(2*b[j], b[j-1], b[j] )
		}
	}
	h = c(1e-14, 1e-09)
	for(i in npro:2)
	{
		h = c(h, as.numeric(a[[i]]))
	}
	return(c(ck + h))
}