#' @export fxa
#' @importFrom stats dbeta dgamma dlnorm dnorm dweibull
#' @importFrom stats pbeta pgamma plnorm pnorm pweibull
#' @importFrom extraDistr dbetapr pbetapr
#' @importFrom copula archmCopula dCopula fgmCopula
#'
fxa = function(z, marg, cofam, xo, tht){
  fm = which(cofam == c("gaussian", "t", "clayton", "gumbel", "frank",
                      "joe", "amh", "fgm"))
  f = numeric(2)
  u = numeric(2)
  for(i in 1:2){
    if (i == 1) k = 1 else k = 2
    if (marg[i] == "weibull") {
      f[i] = dweibull(z[i], shape = xo[i*2], scale = xo[i+k-1])
      u[i] = pweibull(z[i], shape = xo[i*2], scale = xo[i+k-1])
    } else {
      if (marg[i] == "gamma") {
        f[i] = dgamma(z[i], shape = xo[i*2], scale = xo[i+k-1])
        u[i] = pgamma(z[i], shape = xo[i*2], scale = xo[i+k-1])
      } else {
        if (marg[i] == "lnorm") {
          f[i] = dlnorm(z[i], meanlog = xo[i+k-1], sdlog = xo[i*2])
          u[i] = plnorm(z[i], meanlog = xo[i+k-1], sdlog = xo[i*2])
        } else {
          if (marg[i] == "norm") {
            f[i] = dnorm(z[i], mean = xo[i+k-1], sd = xo[i*2])
            u[i] = pnorm(z[i], mean = xo[i+k-1], sd = xo[i*2])
          } else {
            if (marg[i] == "betapr") {
              f[i] = dbetapr(z[i], shape1 = xo[i+k-1], shape2 = xo[i*2])
              u[i] = pbetapr(z[i], shape1 = xo[i+k-1], shape2 = xo[i*2])
            } else {
              if (marg[i] == "beta") {
                f[i] = dbeta(z[i], shape1 = xo[i+k-1], shape2 = xo[i*2])
                u[i] = pbeta(z[i], shape1 = xo[i+k-1], shape2 = xo[i*2])
              }
            }
          }

        }
      }
    }
  }
  # print(u); print(f)
  if (cofam == "amh"){
    kopula =  archmCopula(family = "amh", param = tht)
    return(dCopula(u, kopula)*f[1]*f[2])
  } else {
    if (cofam == "fgm"){
      kopula = fgmCopula(param = tht)
      return(dCopula(u, kopula)*f[1]*f[2])
    } # else {
      # print(paste(BiCopPDF(u[1], u[2], family = fm, par = tht), f[1], f[2]))
    #   return(BiCopPDF(u[1], u[2], family = fm, par = tht)*f[1]*f[2])
    # }
  }
}