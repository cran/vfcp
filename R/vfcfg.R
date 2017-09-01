#' @export vfgumbel
#'
vfgumbel <- function(C, u, tht) {
  exp(-((-log(C))^tht - (-log(u))^tht)^(1/tht))
}

#' @export vfclayton
#'
vfclayton <- function(C, u, tht) {
  (C^(-tht) - u^(-tht) + 1)^(-1/tht)
}

#' @export vffrank
#'
vffrank <- function(C, u, tht) {
  -log(((exp(-C * tht) - 1) * (exp(-tht) - 1))/(exp(-u * tht) - 1) + 1)/tht
}
