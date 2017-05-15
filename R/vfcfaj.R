#' @export vfalihaq
#'
vfalihaq <- function(C, u, tht){
  # Ali–Mikhail–Haq
  return((C/u + C*tht*(1 - u)/u)/(C*tht*(1 - u)/u + 1))
}

#' @export vfjoe
#'
vfjoe <- function(C, u, tht){
  # tht >= 1
  return(1 - (((1 - C)^tht - (1 - u)^tht)/(1 - (1 - u)^tht))^(1/tht))
}
