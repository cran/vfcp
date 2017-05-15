\name{vfalihaq}
\alias{vfalihaq}

\title{
Ali-Mikhail-Haq Copula Variable Given Second One and Copula Probability
}
\description{
\code{v} for Ali-Mikhail-Haq copula \code{C(u, v)} given probability
\code{C(u, v)} and \code{u}.
}
\usage{
vfalihaq(C, u, tht)
}
\arguments{
  \item{C}{
Probability value of the Ali-Mikhail-Haq copula. It can be a vector.
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
