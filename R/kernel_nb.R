#' @title  Kernel Naive Bayes Classifier
#'
#' @description Kernel Naive Bayes Classifier.
#'
#' @param x asd
#' @param y asd
#' @param laplace asd
#' @param bw_thresh asd
#' @param bw_method asd
#' @param bw_adjust asd
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
#' @rdname kernel_nb
#' @export


kernel_nb <- function(
    x,
    y,
    laplace = 0,
    bw_thresh = 1e-8,
    bw_method = "ucv",
    bw_adjust = 1) {

  bw_methods <- c(
    "bcv",
    "nrd",
    "nrd0",
    "SJ",
    "ucv"
  )
  if(!(bw_method %in% bw_methods)) {
    stop(paste0("bw_method should be one of the following: ", paste0(bw_methods, collapse = ", "), "."))
  }

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
  f_bw <- get(paste0("bw.", bw_method))

  if (p_numerics > 0) {
    for (i in 1:k_class) {
      pars_numeric[[i]] <- list()
      pars_numeric[[i]]$bw <- apply(x_classes_numerics[[i]], 2, f_bw)*bw_adjust
      pars_numeric[[i]]$bw[pars_numeric[[i]]$bw == 0 | pars_numeric[[i]]$bw < bw_thresh] <- bw_thresh
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
  class(results) <- "kernel_nb"
  return(results)
}





