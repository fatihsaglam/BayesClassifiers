#' @title  Synthetic Minority Oversampling Technique (SMOTE)
#'
#' @description Resampling with SMOTE.
#'
#' @param x asd
#'
#' @details
#' SMOTE (Chawla et al., 2002) is an oversampling method which creates links
#' between positive samples and nearest neighbors and generates synthetic
#' samples along that link.
#'
#' It is well known that SMOTE is sensitive to noisy data. It may create more
#' noise.
#'
#' @return a list with resampled dataset.
#'  \item{x_new}{Resampled feature matrix.}
#'  \item{y_new}{Resampled target variable.}
#'  \item{C}{Number of synthetic samples for each positive class samples.}
#'
#' @author Fatih Saglam, saglamf89@gmail.com
#'
#'
#' @references
#' Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P. (2002). SMOTE:
#' synthetic minority over-sampling technique. Journal of artificial
#' intelligence research, 16, 321-357.
#'
#'
#' @rdname f_vartypes_x
#' @export
f_vartypes_x <- function(x) {
  var_types <- c()
  k_factors <- c()
  for (i in 1:ncol(x)) {
    var_types[i] <- class(x[,i])
    if (var_types[i] == "character") {
      x[,i] <- as.factor(x[[,i]])
      var_types[i] <- class(x[,i])
    }
    if (var_types[i] == "factor") {
      k_factors[i] <- length(levels(x[,i]))
    }
  }
  i_factors <- which(var_types == "factor")
  i_numerics <- which(var_types != "factor")
  list(
    var_types = var_types,
    i_factors = i_factors,
    i_numerics = i_numerics,
    k_factors = k_factors,
    x = x
  )
}
