#' Confidence interval for alpha (PN)
#'
#' @description Confidence interval for alpha
#'
#' @usage IC.pn(x, model, p)
#'
#'@param x the response vector
#'@param model a variable returned by \code{\link{pn.mle}}
#'@param p confidence level
#'
#' @export



IC.pn <- function(x, model, p){
  alpha <- model$alpha
  dados <- as.matrix(x)
  n <- length(dados)

list(L_Inf = -0.5*qchisq((1-p)/2,2*n)/sum(log(pnorm(x))),
     L_Sup = -0.5*qchisq(1-(1-p)/2,2*n)/sum(log(pnorm(x))))
}


