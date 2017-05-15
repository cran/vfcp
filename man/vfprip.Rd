\name{vfprip}
\alias{vfprip}

\title{
Computation of the vector \code{u} to compute the second vector \code{v}
}
\description{
Auxiliary function. Each vector value \code{u} must be greater
  than the probability of the copula.
}
\usage{
vfprip(Ck, hom, jm)
}

\arguments{
  \item{Ck}{
Copula probability. Single value. Not a vector.
}
  \item{hom}{
Upper value of the \code{u}.
}
  \item{jm}{
Number of segments between \code{hom} and probability of copula.
}
}

\value{
Numeric vector size \code{jm}.
}

\author{
Josef Brejcha
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
