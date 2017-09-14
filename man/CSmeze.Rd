\name{CSmeze}
\alias{CSmeze}
\title{
Defines the area between CDF and Survival for the copula probability
  value
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
  \code{sps} \tab Spline from above and right joining \code{tlc}
    with \code{brc}. \cr
  \code{spc} \tab Spline from the bottom and the left joining
    the \code{tlc} with \code{brc}. \cr
  }
}

\author{
Josef Brejcha
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
