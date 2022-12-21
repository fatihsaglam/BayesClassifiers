#' @title  Synthetic Minority Oversampling Technique (SMOTE)
#'
#' @description Resampling with SMOTE.
#'
#' @param object asd
#' @param newdata asd
#' @param type asd
#' @param ... asd
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
#' @importFrom stats dnorm
#'
#' @references
#' Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P. (2002). SMOTE:
#' synthetic minority over-sampling technique. Journal of artificial
#' intelligence research, 16, 321-357.
#'
#'
#' @rdname predict.gaussian_nb
#' @export

predict.gaussian_nb <- function(object, newdata, type = "pred", ...) {
  if (isFALSE(type %in% c("pred", "prob"))) {
    stop("Type must be pred or prob")
  }

  # read object
  p <- object$p
  i_factors <- object$i_factors
  p_factors <- object$p_factors
  i_numerics <- object$i_numerics
  p_numerics <- object$p_numerics
  class_names <- object$class_names
  k_class <- object$k_class
  priors <- object$priors
  pars_categoric <- object$pars_categoric
  pars_numeric <- object$pars_numeric

  x <- newdata
  n <- nrow(x)
  x_factors <- x[,i_factors]
  x_numerics <- as.matrix(x[,i_numerics])

  likelihood_list <- vector(mode = "list", length = k_class)
  for (i in 1:k_class) {
    likelihood_list[[i]] <- matrix(data = NA, nrow = n, ncol = p)
  }

  # categorical marginal densities
  if (p_factors > 0) {
    for (i in 1:p_factors) {
      cat_names <- names(pars_categoric[[1]]$p[[i]])
      x_factors[,i] <- factor(x_factors[,i], levels = cat_names, labels = cat_names)
      for (j in 1:k_class) {
        likelihood_list[[j]][,i_factors[i]] <- c(pars_categoric[[j]]$p[[i]][as.numeric(x_factors[,i])])
      }
    }
  }

  # numerical gaussian marginal densities
  if (p_numerics > 0) {
    for (i in 1:p_numerics) {
      for (j in 1:k_class) {
        likelihood_list[[j]][,i_numerics[i]] <- dnorm(x = x_numerics[,i], mean = pars_numeric[[j]]$mu[i], sd = pars_numeric[[j]]$sd[i])
      }
    }
  }

  posterior <- sapply(1:k_class, function(m) {
    rowProd(X = likelihood_list[[m]], prior = priors[m])
  })

  if (type == "prob") {
    posterior <- divide_by_rowsum(posterior)
    return(posterior)
  }
  if (type == "pred") {
    predictions <- factor(class_names[max.col(posterior)], levels = class_names, labels = class_names)
    return(predictions)
  }
}
