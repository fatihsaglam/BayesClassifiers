#' @title  Predict Copula Bayes Classifier with Kernel marginals
#'
#' @description Predicts class labels or probabilities for Copula Bayes Classifier with Kernel marginals.
#'
#' @param object asd
#' @param newdata asd
#' @param type asd
#' @param ... asd
#'
#' @details
#' asd
#'
#' @return asd.
#'  \item{asd}{asd}
#'
#' @author Fatih Saglam, saglamf89@gmail.com
#'
#' @importFrom  VineCopula RVinePDF
#' @importFrom  ks kde
#'
#' @rdname predict.copula_kernel_b
#' @export

predict.copula_kernel_b <- function(object, newdata, type = "pred", ...) {

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
  x_classes_numeric <- object$x_classes_numerics
  pars_scale <- object$pars_scale


  x <- newdata
  n <- nrow(x)
  x_factors <- x[,i_factors]
  x_numerics <- as.matrix(x[,i_numerics])
  x_numerics <- scale(x_numerics, pars_scale$mean, pars_scale$sd)
  # scaling into (0,1) range
  x_numerics_scaled <- 1/(1 + exp(-x_numerics))

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

  # numerical kernel marginal densities
  if (p_numerics > 0) {
    c_list <- list()

    for (i in 1:k_class) {
      likelihood_list[[i]][,i_numerics] <- sapply(1:p_numerics, function(m) {
        ks::kde(x = x_classes_numeric[[i]][,m], h = pars_numeric[[i]]$bw[m], eval.points = x_numerics_scaled[,m])$estimate
      })
      ## copula function
      c_list[[i]] <- VineCopula::RVinePDF(newdata = x_numerics_scaled, RVM = pars_numeric[[i]]$copula)
      likelihood_list[[i]] <- cbind(likelihood_list[[i]], c_list[[i]])
    }
  }

  posterior <- sapply(1:k_class, function(m) {
    rowProd(X = likelihood_list[[m]], prior = priors[m])
  })

  if (type == "prob") {
    posterior <- divide_by_rowsum(posterior)
    colnames(posterior) <- class_names
    return(posterior)
  }
  if (type == "pred") {
    predictions <- factor(class_names[max.col(posterior)], levels = class_names, labels = class_names)
    return(predictions)
  }
}
