#' @title  Predict Clustered Kernel Naive Bayes
#'
#' @description Predicts class labels or probabilities for Clustered Kernel Naive Bayes.
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
#'
#' @rdname predict.cl_kernel_nb
#' @export


predict.cl_kernel_nb <- function(object, newdata, type = "pred", ...) {

  if (isFALSE(type %in% c("pred", "prob"))) {
    stop("Type must be pred or prob")
  }

  # read object
  k_class_original <- object$k_class_original
  class_names_new <- object$class_names_new
  class_names_original <- object$class_names_original

  x <- newdata
  n <- nrow(x)

  m_prob_cluster <- predict.kernel_nb(object = object, newdata = x, type = "prob")

  posterior <- matrix(data = NA, nrow = n, ncol = k_class_original)

  for (i in 1:k_class_original) {
    posterior[,i] <- rowSums(m_prob_cluster[,class_names_new[[i]], drop = FALSE])
  }

  if (type == "prob") {
    posterior <- divide_by_rowsum(posterior)
    colnames(posterior) <- class_names_original
    return(posterior)
  }

  if (type == "pred") {
    predictions <- factor(class_names_original[max.col(posterior)],
                          levels = class_names_original,
                          labels = class_names_original)
    return(predictions)
  }
}
