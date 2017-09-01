\name{vfpint}
\alias{vfpint}
\title{
Integrating the probability density within a range defined
  in the \code{\link{CSmeze}}.
}
\description{
Integration of the bivariate probability density on a copula basis
  in the area defined in the \code{\link{CSmeze}}.
}
\usage{
vfpint(p, k, mzu, marg, fam, xo, tht)
}
\arguments{
  \item{p}{
Copula probability vektor.
}
  \item{k}{
Index in \code{p}.
}
  \item{mzu}{
Output list from \code{\link{CSmeze}}
}
  \item{marg}{
Character vector size2. Now possible vector components: \cr
  "weibull", "gamma", "lnorm", "norm", "beta", "betapr".
}
  \item{fam}{
A name of copula: "clayton", "frank", "gumbel", "joe".
}
  \item{xo}{
Numeric vector size 4. Its components: \cr
  \tabular{rl}{
  \code{xo[1]}, \code{xo[3]} \tab scale, meanlog, mean, shape1 \cr
  \code{xo[2]}, \code{xo[4]} \tab shape, sdlog, sd, shape2 \cr
  }
}
  \item{tht}{
Copula parameter.
}
}
\value{
Approximate \code{p[k]}.
}

\author{
Josef Brejcha
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
