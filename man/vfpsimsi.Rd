\name{vfpsimsi}
\alias{vfpsimsi}

\title{
Probability of the area by the Monte Carlo method
}
\description{
The area defined by the splines. Probability of area by simulation.
}
\usage{
vfpsimsi(p, k, mzs, marg, fam, xo, tht, no)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
Copula probability vektor.
}
  \item{k}{
Index in \code{p}.
}
  \item{mzs}{
Output list from \code{\link{CSmezesi}}
}
  \item{marg}{
Character vector size2. Now possible vector components: \cr
  "weibull", "gamma", "lnorm", "norm", "beta", "betapr".
}
  \item{fam}{
A name of copula: "clayton", "frank", "gumbel", "joe",
  "amh", "fgm".
}
  \item{xo}{
a list; see \code{\link{mvdc}} in \code{copula}
}
  \item{tht}{
copula parameter
}
  \item{no}{
number of attempts
}
}

\value{
probability
}

\author{
Josef Brejcha
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
