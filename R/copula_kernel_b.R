#' @title  Copula Bayes Classifier with Kernel marginals
#'
#' @description Copula Bayes Classifier with Kernel marginals.
#'
#' @param x asd
#' @param y asd
#' @param sd_thresh asd
#' @param laplace asd
#' @param bw_thresh asd
#' @param bw_method asd
#' @param bw_adjust asd
#' @param cores asd
#' @param familyset asd
#' @param rotations asd
#' @param margin asd
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
#' @importFrom  VineCopula RVineStructureSelect
#'
#' @rdname copula_kernel_b
#' @export


copula_kernel_b <- function(
    x,
    y,
    sd_thresh = 1e-8,
    laplace = 0,
    bw_thresh = 1e-8,
    bw_method = "ucv",
    bw_adjust = 1,
    cores = 1,
    familyset = c(0,1,3,4,5,6),
    rotations = FALSE,
    margin = 1e-3) {

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

  x_numerics <- as.matrix(x[,i_numerics, drop = FALSE])

  pars_scale <- list()

  pars_scale$mean <- apply(x_numerics, 2, mean)
  pars_scale$sd <- apply(x_numerics, 2, sd) + sd_thresh

  x_numerics <- scale(x_numerics, pars_scale$mean, pars_scale$sd)
  # scaling into (0,1) range
  x_numerics_scaled <- 1/(1 + exp(-x_numerics))
  x[,i_numerics] <- x_numerics_scaled

  p_numerics <- ncol(x_numerics_scaled)

  x_classes_numerics <- lapply(class_names, function(m) x_numerics_scaled[y == m,])

  results <- kernel_nb(x = x, y = y, laplace = laplace, bw_thresh = bw_thresh, bw_method = bw_method, bw_adjust = bw_adjust)

  if (p_numerics < 2) {
    stop("Copula densities need at least two numerical variable.")
  }

  for (i in 1:k_class) {
    ### copula parameters

    results$pars_numeric[[i]]$copula <- VineCopula::RVineStructureSelect(
      data = x_classes_numerics[[i]],
      familyset = familyset ,
      cores = cores,
      rotations = rotations)
  }

  results$pars_scale <- pars_scale

  class(results) <- "copula_kernel_b"
  return(results)
}
