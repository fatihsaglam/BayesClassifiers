#' @title  Divide row by row sum
#'
#' @description Divide row by row sum.
#'
#' @param X asd
#'
#' @details
#' asd
#'
#' @return asd.
#'  \item{asd}{asd}
#'
#' @author Fatih Saglam, saglamf89@gmail.com
#'
#' @rdname divide_by_rowsum
#' @export

divide_by_rowsum <- function(X) {
  rcpp_divide_by_rowsum(X = X)
}


