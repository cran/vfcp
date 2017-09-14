\name{CSmezesi}
\alias{CSmezesi}

\title{
Defines the area between CDF and Survival for the copula probability
  value
}
\description{
The area defines the left and bottom line of the cumulative distribution function, the CDF, and the right and top line of the survival function. Both functions have the same probability.
}
\usage{
CSmezesi(cx, pro, xo, marg, cofam, tht)
}

\arguments{
  \item{cx}{
Copula probability. Single value.
}
  \item{pro}{
Numeric vector. Its \code{pro[1:k]} are upper values of the \code{u}.
  Next \code{pro[-c(1:k)]} are then all greater than or equal to 1.
}
  \item{xo}{
Numeric vector size 4. Its components: \cr
  \tabular{rl}{
  \code{xo[1]}, \code{xo[3]} \tab scale, meanlog, mean, shape1 \cr
  \code{xo[2]}, \code{xo[4]} \tab shape, sdlog, sd, shape2 \cr
  }
}
  \item{marg}{
Character vector size2. Now possible vector components: \cr
  "weibull", "gamma", "lnorm", "norm", "beta", "betapr".
}
  \item{cofam}{
A name of copula: "clayton", "frank", "gumbel", "joe",
  "amh", "fgm".
}
  \item{tht}{
Copula parameter.
}
}

\value{
A list with components as follows: \cr
  \tabular{rl}{
  \code{tlc} \tab The upper left intersection of both CDF and
    survival lines. \cr
  \code{brc} \tab The bottom right intersection of both CDF and
    survival lines. \cr
  \code{sp} \tab A pair of numeric vectors \code{x} and \code{y}
    to create the survival \code{sps} spline in \code{\link{vfpsimsi}}. \cr
  \code{cp} \tab A pair of numeric vectors \code{x} and \code{y}
    to create the CDF \code{spc} spline in \code{\link{vfpsimsi}}. \cr
  }
}

\author{
Josef Brejcha
}
\examples{
tht = 5.6
cx = c(0.05, 0.1, 0.15, 0.25)
pro = c(0.99999, 0.9999, 0.999, 0.99, 24, 16, 8, 4)
fam = "gumbel"
marg = c("weibull", "betapr")
xo = c(200, 2.75, 16.5, 6.60)
e12 = vfeduo(marg, xo)
p = numeric(length(cx))
x12 = qweibull(0.975, scale = xo[1], shape = xo[2])
y12 = qbetapr(0.975, shape1 = xo[3], shape2 = xo[4])
mtit = paste(fam, " ... ", marg[1], "(", xo[1], ", ", xo[2], ")",
						 " ", marg[2], "(", xo[3], ", ", xo[4], ")",
						 sep = "")
plot(NULL, NULL, xlim = c(0, x12), ylim = c(0, y12),
     xlab = paste("x, E[x] = ", round(e12[1], 2)),
		 ylab = paste("y, E[y] = ", round(e12[2], 2)),
		 main = mtit)
points(e12[1], e12[2], pch = 20)
abline(h = e12[2], v = e12[1])
grid(col = "grey50")
#===========================
no = 100000
for (k in 1:length(cx)){
	mzu = CSmezesi(cx[k], pro, xo, marg, fam, tht)
	sae = vfpsae(rod = marg, xo)
	lines(mzu$sp, col = k)
	lines(mzu$cp, col = k)
	# p[k] = vfpint(p, k, mzu, marg, fam, xo, tht)
	p[k] = vfpsimsi(p, k, mzs = mzu, marg, fam, xo = sae, tht, no)
}
gd = paste(c("C", format(cx, 4)), c("       p" , round(p, 3)))
legend("topright", legend = gd, text.col = c(1, 1:length(p)),bty = "n")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
