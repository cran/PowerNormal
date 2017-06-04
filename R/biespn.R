#' Unbiased estimator for alpha (PN)
#'
#' @description Unbiased estimator for \code{alpha}
#'
#' @usage bias.pn(x, model)
#'
#'@param x the response vector
#'@param model a variable returned by \code{\link{pn.mle}}
#'
#' @export



bias.pn <- function(x, model){
  alpha <- model$alpha
  dados <- as.matrix(x)
  n <- length(dados)



list(alpha_cor=alpha*(n-1)/n, loglik = .lv_pn(alpha*(n-1)/n,x))
}


