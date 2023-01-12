#' @title  Multivariate Gaussian Bayes Classifier
#'
#' @description Multivariate Gaussian Bayes Classifier.
#'
#' @param x asd
#' @param y asd
#' @param laplace asd
#' @param Sigma_equal asd
#'
#' @details
#' asd
#'
#' @return asd.
#'  \item{asd}{asd}
#'
#' @author Fatih Saglam, saglamf89@gmail.com
#'
#' @importFrom stats cov
#'
#' @rdname mvgaussian_b
#' @export


mvgaussian_b <- function(
    x,
    y,
    laplace = 0,
    Sigma_equal = FALSE) {

  p <- ncol(x)
  n <- nrow(x)

  if (!is.factor(y)) {
    y <- as.factor(y)
  }

  class_names <- levels(y)
  k_class <- length(class_names)
  n_classes <- sapply(class_names, function(m) sum(y == m))

  m_vartypes_x <- f_vartypes_x(x = x)
  i_factors <- m_vartypes_x$i_factors
  i_numerics <- m_vartypes_x$i_numerics
  x <- m_vartypes_x$x
  k_factors <- m_vartypes_x$k_factors

  x_numerics <- x[,i_numerics, drop = FALSE]
  p_numerics <- ncol(x_numerics)
  x_factors <- x[,i_factors, drop = FALSE]
  p_factors <- ncol(x_factors)

  x_classes_numerics <- lapply(class_names, function(m) x_numerics[y == m,])
  x_classes_factors <- lapply(class_names, function(m) x_factors[y == m,])

  # prior estimation
  priors <- (n_classes + laplace)/(n + laplace*k_class)

  # likelihood parameter estimations
  ## categorical variables
  pars_categoric <- list()
  if (p_factors > 0) {
    for (i in 1:k_class) {
      pars_categoric[[i]] <- list()
      pars_categoric[[i]]$p <- list()
      for (j in 1:p_factors) {
        pars_categoric[[i]]$p[[j]] <- (table(x_classes_factors[[i]][,j]) + laplace)/(n_classes[i] + laplace*k_factors[j])
      }
      names(pars_categoric[[i]]$p) <- colnames(x_factors)
    }
    names(pars_categoric) <- class_names
  }

  ## numerical variables
  pars_numeric <- list()

  if (p_numerics > 0) {
    for (i in 1:k_class) {
      pars_numeric[[i]] <- list()
      pars_numeric[[i]]$mu <- colMeans(x_classes_numerics[[i]])
      if (Sigma_equal) {
        pars_numeric[[i]]$Sigma <- cov(x_numerics)
      } else {
        pars_numeric[[i]]$Sigma <- cov(x_classes_numerics[[i]])
      }
    }
    names(pars_numeric) <- class_names
  }

  results <- list(
    p = p,
    i_factors = i_factors,
    p_factors = p_factors,
    i_numerics = i_numerics,
    p_numerics = p_numerics,
    class_names = class_names,
    k_class = k_class,
    priors = priors,
    k_factors = k_factors,
    pars_categoric = pars_categoric,
    pars_numeric = pars_numeric,
    x_classes_numerics = x_classes_numerics
  )
  class(results) <- "mvgaussian_b"
  return(results)
}

