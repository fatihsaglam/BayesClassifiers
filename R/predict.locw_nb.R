#' @title  Predict Locally Weighted Naive Bayes
#'
#' @description Predicts class labels or probabilities for Locally Weighted Naive Bayes.
#'
#' @param object asd
#' @param newdata asd
#' @param type asd
#' @param sd_thresh asd
#' @param laplace asd
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
#' @importFrom mvnfast dmvn
#'
#' @rdname predict.locw_nb
#' @export
#'
predict.locw_nb <- function(object, newdata, type = "prob", sd_thresh = 1e-8, laplace = 0){
  n <- object$n
  p <- object$p
  x <- object$x
  y <- object$y

  n_classes <- object$n_classes
  k_class <- object$k_class
  class_names <- object$class_names
  k <- object$k

  x_test <- newdata
  n_test <- nrow(x_test)

  posteriors_all <- matrix(NA, nrow = n_test, ncol = k_class)

  for (i in 1:n_test) {
    x_selected <- x_test[i,,drop = FALSE]

    m_nn <- RANN::nn2(data = as.matrix(x), query = as.matrix(x_selected), k = pmin(k, n))
    nn_dists <- m_nn$nn.dists
    nn_index <- m_nn$nn.idx

    x_selected <- x[nn_index,]
    y_selected <- y[nn_index]

    weights <- nn_dists/max(nn_dists)
    weights <- weights*k/sum(weights) ### niyesini anlamadım. ağırlıkların toplamını k yapıyor.

    x_selected_classes <- lapply(class_names, function(m) x_selected[y_selected == m,,drop = FALSE])

    weights_classes <- lapply(class_names, function(m) weights[y_selected == m])

    means <- lapply(1:k_class, function(m2) sapply(1:p, function(m) {
      ww <- weights_classes[[m2]]/sum(weights_classes[[m2]])*n_test
      Hmisc::wtd.mean(x = x_selected_classes[[m2]][,m,drop = FALSE], weights = ww, na.rm = TRUE)
    }))

    stds <- lapply(1:k_class, function(m2) sapply(1:p, function(m) {
      ww <- weights_classes[[m2]]/sum(weights_classes[[m2]])*n_test
      sdsd <- sqrt(Hmisc::wtd.var(x = x_selected_classes[[m2]][,m,drop = FALSE], weights = ww, na.rm = TRUE))
      sdsd[sdsd == 0 | sdsd < sd_thresh | is.na(sdsd)] <- sd_thresh
      return(sdsd)
    }))

    priors <- (sapply(weights_classes, sum) + laplace)/(sum(weights) + laplace*k_class)

    likelihoods <- lapply(1:k_class, function(m) {
      ll <- sapply(1:p, function(mm) {
        dnorm(x = x_selected[,mm], mean = means[[m]], sd = stds[[m]])
      })

      ll[is.na(ll)] <- 0
      return(ll)
    })

    posteriors <- sapply(1:k_class, function(m) {
      apply(cbind(priors[m], likelihoods[[m]]), 1, prod)
    })

    if(all(posteriors == 0)){
      posteriors <- runif(length(posteriors), min = 0.49, max = 0.51)
    }
    posteriors[is.infinite(posteriors)] <- .Machine$double.xmax
    posteriors <- posteriors/sum(posteriors)


    posteriors_all[i,] <- posteriors
  }

  colnames(posteriors_all) <- class_names

  if (type == "prob") {
    return(posteriors_all)
  }
  if (type == "pred") {
    predictions <- apply(posteriors_all, 1, function(m) class_names[which.max(m)])
    return(predictions)
  }
}
