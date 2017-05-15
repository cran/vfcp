\name{vffgm}
\alias{vffgm}

\title{
Farlie-Gumbel-Morgenstern Copula Variable Given Second One and
Copula Probability
}
\description{
\code{v} for Farlie-Gumbel-Morgenstern copula \code{C(u, v)}
given probability \code{C(u, v)} and \code{u}.
}
\usage{
vffgm(C, u, tht)
}
\arguments{
  \item{C}{
Probability value of the Farlie-Gumbel-Morgenstern copula.
It can be a vector.
}
  \item{u}{
The first variable value of the \code{C(u, v)}.
  \code{u} can be a vector if \code{C} is a single.
  \code{u} is a matrix with \code{nrow = length(C)} if \code{C}
    is a vector.
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
\references{
A.K. SUZUKI, F. LOUZADA and V.G. CANCHO, On estimation and
influence diagnostics for a Bivariate Promotion Lifetime Model
Based on the FGM Copula: A Fully Bayesian Computation, \emph{Tendˆencias em Matem´ atica Aplicada e Computacional, 14, N. 3 (2013), 441-461}, \url{http://www.scielo.br/pdf/tema/v14n3/a14v14n3.pdf}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
