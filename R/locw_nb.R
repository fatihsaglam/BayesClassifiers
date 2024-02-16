#' @title  Locally Weighted Naive Bayes
#'
#' @description Locally Weighted Naive Bayes.
#'
#' @param x asd
#' @param y asd
#' @param k = asd,
#'
#' @details
#' asd
#'
#' @return asd.
#'  \item{asd}{asd}
#'
#' @author Fatih Saglam, saglamf89@gmail.com
#'
#' @importFrom  stats sd
#'
#' @rdname locw_nb
#' @export

locw_nb <- function(x, y, k = 50){
  n <- nrow(x)
  p <- ncol(x)

  class_names <- unique(y)
  k_class <- length(class_names)

  n <- nrow(x)
  n_classes <- sapply(class_names, function(m) sum(y == m))

  results <- list(n = n,
                  p = p,
                  x = x,
                  y = y,
                  n_classes = n_classes,
                  k_class = k_class,
                  class_names = class_names,
                  k = k)
  class(results) <- "locw_nb"
  return(results)
}
