\name{vfmargi}
\alias{vfmargi}

\title{
Auxilliary function.
}
\description{
Auxiliary function used in \code{vfplot}.
}
\usage{
vfmargi(cx, k, hor, dol, fam, marg, cdf, xo, tht)
}

\arguments{
  \item{cx}{
A vector of copula probabilities.
}
  \item{k}{
An index in the \code{cx}.
}
  \item{hor}{
Upper value of the \code{u}.
}
  \item{dol}{
Number of segments between \code{hor} and probability of copula.
}
 \item{fam}{
character; A name of copula. One of c("clayton", "frank", "gumbel",
    "amh", "joe", "fgm"). "amh", "joe", "fgm" names are for
    Ali-Mikhail-Haq, Joe, Farlie-Gumbel-Morgenstern copulas.
}
 \item{marg}{
A vector size 2. Combination of these marginals: \cr
  c("weibull", "gamma", "lnorm", "norm").
}
 \item{cdf}{
logical; Computation for CDF when TRUE. If FALSE
  is the same for Survival.
}
  \item{xo}{
A vector of marginal distribution parameters. It is size 4
  with these components: \cr
\tabular{rl}{
  \code{xo[1], xo[3]} \tab {scale, meanlog, mean} \cr
  \code{xo[2], xo[4]} \tab {shape, sdlog, sd} \cr
}
}
  \item{tht}{
copula parameter
}
}

\value{
A list of the following components: \cr
\tabular{rl}{
  \code{gweo} \tab {Horizontal coordinates of the contour.} \cr
  \code{glno} \tab {Vertical coordinates of the contour.} \cr
}
}

\author{
Josef Brejcha
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
