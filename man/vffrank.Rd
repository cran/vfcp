\name{vffrank}
\alias{vffrank}
\title{
Frank Copula Variable Given Second One and Copula Probability
}
\description{
\code{v} for Frank copula \code{C(u, v)} given probability
\code{C(u, v)} and \code{u}.
}
\usage{
vffrank(C, u, tht)
}
\arguments{
  \item{C}{
Probability value of the Frank copula. It can be a vector.
}
  \item{u}{
The first variable value of the \code{C(u, v)}.
  \code{u} can be a vector if \code{C} is a single.
  \code{u} is a matrix with \code{nrow = length(C)} if \code{C} is a vector.
}
  \item{tht}{
Copula parameter
}
}
\details{
The value of the \code{u} must be grater than \code{C}.
}
\value{
The value of the second variable depending on the first variable
and copula probability value.
}
\author{
Josef Brejcha
}
\examples{
require(VineCopula)
C <- 0.3
tht <- 6
u <- c(0.35, 0.4, 0.45)
v <- vffrank(C, u, tht)
BiCopCDF(u, v, family = 5, par = tht)
##
## CDF Frank ##
tht = 5
cx = seq(0.1, 0.9, 0.1)
nC = length(cx)
hor = 1 - 1e-13
dol = 50
tit = paste("CDF: frank ... tht =", tht)
plot(x = NULL, y = NULL, xlim = c(0, 1), ylim = c(0, 1),
		 xlab = "u", ylab = "v", type = "n", main = tit)
for(k in 1:nC){
  u = vfprip(Ck = cx[k], hom = hor, jm = dol)
  v = vffrank(cx[k], u, tht)
  lines(u, v, col = k)
}
legend("bottomright", legend = cx, text.col = 1:nC, bty = "n")
grid()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
