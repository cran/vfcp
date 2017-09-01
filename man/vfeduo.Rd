\name{vfeduo}
\alias{vfeduo}

\title{
Expected values of marginal distributions
}
\description{
Auxiliary function that calculates the expected values of marginal
  distributions.
}
\usage{
vfeduo(marg, xo)
}

\arguments{
  \item{marg}{
Character vector size 2. It components now can be c("weibull",
  "gamma", "lnorm", "norm", "betapr", "beta").
}
  \item{xo}{
Vector size 4 of parameters of \code{marg}. \cr
  \tabular{rl}{
  \code{xo[1], xo[3]} \tab {\code{scale}, \code{meanlog},
    \code{mean}, \code{shape1}} \cr
  \code{xo[2], xo[4]} \tab {\code{shape}, \code{sdlog},
    \code{sd}, \code{shape2}} \cr
  }
}
}

\value{
Numeric vector size 2.
}

\author{
Josef Brejcha
}

\examples{
vfeduo(marg = c("betapr", "beta"), xo = c(5, 5, 3, 20))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
