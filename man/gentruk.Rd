\name{gentruk}
\alias{gentruk}
\title{
Creating an object for CDF and copula survival
}
\description{
For given inputs, the coordinates of the object defined by the CDF
  and the survival function for the copula object are created.
}
\usage{
gentruk(tht, fm, C, pro)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tht}{
Copula parameter. If \code{fam = "fgm"}, it must be a vector
  of size \eqn{dm*(dm-1)/2 + 1}.
}
  \item{fm}{
Family name copula. These can be: "clayton", "gumbel", "frank",
  "joe", "amh", "fgm".
}
  \item{C}{
Probability value of the copula. Single value.
}
  \item{pro}{
Numeric vector. Its \code{pro[1:k]} are upper values of the \code{u}.
  Next \code{pro[-c(1:k)]} are then all greater than or equal to 1.
}
}
\value{
A list with components as \code{\link{trimeze}} value.
}
\author{
Josef Brejcha
}
\examples{
tht = 0.6
cx = c(0.025, 0.05, 0.1, 0.15, 0.25)
pro = c(0.99999, 0.9999, 0.999, 0.99, 24, 16, 8, 4)
dm = 2
fam = "fgm"
marg = c("weibull", "betapr")
xo = c(200, 2.75, 16.5, 6.60)
e12 = vfenuo(marg, xo)
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
kop2 = kopula(fam, tht, dm)
fmc = c("", "", "clayton", "gumbel", "frank", "joe")
pro = c(0.999999, 0.99999, 0.9999, 16, 8, 4, 2)
tm3 = list()
tmk = list()
for (k in 1:length(cx)){
  tm3 = gentruk(tht, fm=fam, C=cx[k], pro)
  tmk[[k]] = tm3
}
p = prosim(C = cx, fam, tht, dm, no = 100000)
# =============
xa = c("u")
ya = c("v")
for (k in 1:length(cx)){
  mspx = vfmrg(rdj=marg, i=1, cosi=tmk[[k]]$sp$s1, yo=xo, cdf=TRUE)
  mspy = vfmrg(rdj=marg, i=2, cosi=tmk[[k]]$sp$s2, yo=xo, cdf=TRUE)
  mcpx = vfmrg(rdj=marg, i=1, cosi=tmk[[k]]$cp$c1, yo=xo, cdf=TRUE)
  mcpy = vfmrg(rdj=marg, i=2, cosi=tmk[[k]]$cp$c2, yo=xo, cdf=TRUE)
  lines(mspx,mspy, col=k)
  lines(mcpx, mcpy, col = k)
}
  legend("topleft", legend = c("C", cx), text.col = c(1, 1:length(cx)),
         bty = "n")
  legend("topright", legend = c("p", round(p, 4)),
         text.col = c(1, 1:length(cx)), bty = "n")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
