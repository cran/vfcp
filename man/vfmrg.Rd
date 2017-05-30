\name{vfmrg}
\alias{vfmrg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Auxiliary function
}
\description{
Auxiliary function used in \code{plotmuvw}. It computes random
  variable value of the CDF or survival which can be
  one of the c("weibull", "gamma", "lnorm", "norm").
}
\usage{
vfmrg(rdj, i, cosi, yo, cdf)
}

\arguments{
  \item{rdj}{
A character vector size 3. Its components are from
  c("weibull", "gamma", "lnorm", "norm").
}
  \item{i}{
An index of the \code{rdj}
}
  \item{cosi}{
A vector of probabilities
}
  \item{yo}{
Vector size 6 of parameters of \code{rdj}
}
  \item{cdf}{
Cumulative distribution function when \code{TRUE}, survival otherwise.
}
}

\value{
Numeric vector
}

\author{
Josef Brejcha
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
