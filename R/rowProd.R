#' @title  Row products
#'
#' @description Row products
#'
#' @param X asd
#' @param prior asd
#'
#' @details
#' asd
#'
#' @return asd.
#'  \item{asd}{asd.}
#'
#' @author Fatih Saglam, saglamf89@gmail.com
#'
#' @rdname rowProd
#' @export


rowProd <- function(X, prior = rep(1, ncol(X))) {
  rcpp_rowProd(X = X, prior = prior)
}
