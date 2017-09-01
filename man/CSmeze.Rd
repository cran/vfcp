\name{CSmeze}
\alias{CSmeze}
\title{
Defines the area between CDF and Survival for the copula probability
  value.
}
\description{
The area defines the left and bottom line of the cumulative distribution function, the CDF, and the right and top line of the survival function. Both functions have the same probability.
}
\usage{
CSmeze(cx, pro, xo, marg, cofam, tht)
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
A name of copula: "gaussian", "clayton", "frank", "gumbel", "joe",
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
  \code{sps} \tab Spline from above and right joining \code{tlc}
    with \code{brc}. \cr
  \code{spc} \tab Spline from the bottom and the left joining
    the \code{tlc} with \code{brc}. \cr
  }
}

\author{
Josef Brejcha
}
\examples{
tht = 5.6
cx = c(0.275)
pro = c(0.999, 0.99, 16, 8, 4)
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
for (k in 1:length(cx)){
	mzu = CSmeze(cx[k], pro, xo, marg, cofam = fam, tht)
	lines(mzu$sps, col = k)
	lines(mzu$spc, col = k)
	p[k] = vfpint(p, k, mzu, marg, fam, xo, tht)
}
gd = paste(c("C", format(cx, 4)), c("       p" , round(p, 4)))
legend("topright", legend = gd, text.col = c(1, 1:length(p)),bty = "n")
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
