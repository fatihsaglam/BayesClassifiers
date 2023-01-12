#' @title  Clustered Copula Bayes Classifier with Gaussian marginals
#'
#' @description Clustered Copula Bayes Classifier with Gaussian marginals.
#'
#' @param x asd
#' @param y asd
#' @param var_equal asd
#' @param laplace asd
#' @param sd_thresh asd
#' @param cores asd
#' @param familyset asd
#' @param rotations asd
#' @param margin asd
#' @param max_cluster = 9,
#' @param eps = .Machine$double.eps,
#' @param tol = c(1.e-5, sqrt(.Machine$double.eps)),
#' @param itmax = c(.Machine$integer.max, .Machine$integer.max)
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
#' @rdname cl_copula_gaussian_b
#' @export

cl_copula_gaussian_b <- function(
    x,
    y,
    var_equal = FALSE,
    laplace = 0,
    sd_thresh = 1e-8,
    cores = 1,
    familyset  = c(0,1,3,4,5,6),
    rotations = FALSE,
    margin = 1e-3,
    max_cluster = 9,
    eps = .Machine$double.eps,
    tol = c(1.e-5, sqrt(.Machine$double.eps)),
    itmax = c(.Machine$integer.max, .Machine$integer.max)) {

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

  m_cluster_list <- lapply(x_classes_numerics, function(m) {
    m_mclust <- Mclust(data = m,
                       G = 1:max_cluster,
                       modelNames = "VVV",
                       verbose = FALSE,
                       control = emControl(eps = eps,
                                           tol = tol,
                                           itmax = itmax))
    while (any(table(m_mclust$classification) < p)) {
      m_mclust <- Mclust(data = m, G = m_mclust$G - 1, modelNames = "VVV", verbose = FALSE,
                         control = emControl(eps = eps,
                                             tol = tol,
                                             itmax = itmax))
    }
    return(m_mclust)
  })

  y_new <- as.character(y)
  class_names_new <- list()
  for (i in 1:k_class) {
    y_new[y_new == class_names[i]] <- paste0(class_names[i], "_", m_cluster_list[[i]]$classification)
    class_names_new[[i]] <- paste0(class_names[i], "_", 1:m_cluster_list[[i]]$G)
  }
  y_new <- factor(y_new, levels = unlist(class_names_new), labels = unlist(class_names_new))

  results <- copula_gaussian_b(x = x, y = y_new, laplace = laplace,
                               sd_thresh = sd_thresh, var_equal = var_equal,
                               cores = cores, familyset = familyset,
                               rotations = rotations, margin = margin)
  results$m_cluster_list <- m_cluster_list
  results$class_names_new <- class_names_new
  results$k_class_original <- k_class
  results$class_names_original <- class_names

  class(results) <- "cl_copula_gaussian_b"
  return(results)
}
