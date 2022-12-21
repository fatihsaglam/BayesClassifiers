#' @title  Synthetic Minority Oversampling Technique (SMOTE)
#'
#' @description Resampling with SMOTE.
#'
#' @param x asd
#' @param y asd
#' @param laplace asd
#' @param sd_thresh asd
#' @param var_equal asd
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
#'  \item{x}{asd}
#'  \item{y}{asd}
#'
#' @author Fatih Saglam, saglamf89@gmail.com
#'
#' @importFrom  stats sd
#'
#' @references
#' Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P. (2002). SMOTE:
#' synthetic minority over-sampling technique. Journal of artificial
#' intelligence research, 16, 321-357.
#'
#' @rdname gaussian_nb
#' @export

gaussian_nb <- function(
    x,
    y,
    laplace = 0,
    sd_thresh = 1e-8,
    var_equal = FALSE) {

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
  pars_categoric

  ## numerical variables
  pars_numeric <- list()

  if (p_numerics > 0) {
    if (var_equal) {
      sd_all <- apply(x_numerics, 2, sd)
      sd_all[sd_all == 0 | sd_all < sd_thresh] <- sd_thresh
    }

    for (i in 1:k_class) {
      pars_numeric[[i]] <- list()
      pars_numeric[[i]]$mu <- apply(x_classes_numerics[[i]], 2, mean)
      if (var_equal) {
        pars_numeric[[i]]$sd <- sd_all
      } else {
        pars_numeric[[i]]$sd <- apply(x_classes_numerics[[i]], 2, sd)
        pars_numeric[[i]]$sd[pars_numeric[[i]]$sd == 0 | pars_numeric[[i]]$sd < sd_thresh] <- sd_thresh
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
    pars_numeric = pars_numeric
  )
  class(results) <- "gaussian_nb"
  return(results)
}
