\name{fxa}
\alias{fxa}
\title{
Joint probability density function of two variables based on the cupola
}
\description{
Joint probability density function of two variables based on
  the cupola. The probability is calculated only at one point
  of the marginal distributions.
}
\usage{
fxa(z, marg, cofam, xo, tht)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{
Numeric vector of size 2.
}
  \item{marg}{
Character vector size2. Now possible vector components: \cr
  "weibull", "gamma", "lnorm", "norm", "beta", "betapr".
}
  \item{cofam}{
A name of copula: "clayton", "frank", "gumbel", "joe",
  "amh", "fgm". \cr
}
  \item{xo}{
Numeric vector size 4. Its components: \cr
  \tabular{rl}{
  \code{xo[1]}, \code{xo[3]} \tab scale, meanlog, mean, shape1 \cr
  \code{xo[2]}, \code{xo[4]} \tab shape, sdlog, sd, shape2 \cr
  }
}
  \item{tht}{
Copula parameter
}
}
\details{
"betapr" is the name for 'inverted beta' alias also 'beta prime'.
}
\value{
Single numeric value.
}

\author{
Josef Brejcha
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
