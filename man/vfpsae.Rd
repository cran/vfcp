\name{vfpsae}
\alias{vfpsae}

\title{
Auxiliary function
}
\description{
Converts \code{xo} to \code{sae}.
}
\usage{
vfpsae(rod, xo)
}

\arguments{
  \item{rod}{
Character vector size2. Now possible vector components: \cr
  "weibull", "gamma", "lnorm", "norm", "beta", "betapr".
}
  \item{xo}{
Numeric vector size 4. Its components: \cr
  \tabular{rl}{
  \code{xo[1]}, \code{xo[3]} \tab scale, meanlog, mean, shape1 \cr
  \code{xo[2]}, \code{xo[4]} \tab shape, sdlog, sd, shape2 \cr
  }
}
}

\value{
A list having the structure required by \code{\link{mvdc}} in
  \code{\link{vfpsimsi}}.
}

\author{
Josef Brejcha
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
