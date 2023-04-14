locw_nb <- function(x, y, k = 50){
  n <- nrow(x)

  class_names <- unique(y)
  k_classes <- length(class_names)

  n <- nrow(x)
  n_classes <- sapply(class_names, function(m) sum(y == m))

  priors_normal <- n_classes/n

  results <- list(n = n,
                  x = x,
                  y = y,
                  n_classes = n_classes,
                  k_classes = k_classes,
                  class_names = class_names,
                  k = k)
  class(results) <- "locw_nb"
  return(results)
}

predict.locw_nb <- function(object, newdata, type = "prob"){
  n <- object$n
  x <- object$x
  y <- object$y
  n_classes <- object$n_classes
  k_classes <- object$k_classes
  class_names <- object$class_names
  k <- object$k

  x_test <- newdata
  n_test <- nrow(x_test)

  posteriors_all <- matrix(NA, nrow = n_test, ncol = k_classes)

  for (i in 1:n_test) {
    x_selected <- x_test[i,,drop = FALSE]

    m_nb <- RANN::nn2(data = as.matrix(x), query = as.matrix(x_selected), k = pmin(k, n))
    nb_dists <- m_nb$nn.dists
    nb_index <- m_nb$nn.idx

    x_selected <- x[nb_index,]
    y_selected <- y[nb_index]

    weights <- nb_dists/max(nb_dists)

    weights <- weights*k/sum(weights) ### niyesini anlamadım. ağırlıkların toplamını k yapıyor.
    x_selected_classes <- lapply(class_names, function(m) x_selected[y_selected == m,,drop = FALSE])

    weights_classes <- lapply(class_names, function(m) weights[y_selected == m])

    means <- lapply(1:k_classes, function(m2) sapply(1:p, function(m) {
      ww <- weights_classes[[m2]]/sum(weights_classes[[m2]])*n_test
      Hmisc::wtd.mean(x = x_selected_classes[[m2]][,m,drop = FALSE], weights = ww, na.rm = TRUE)
    }))

    stds <- lapply(1:k_classes, function(m2) sapply(1:p, function(m) {
      ww <- weights_classes[[m2]]/sum(weights_classes[[m2]])*n_test
      sdsd <- sqrt(Hmisc::wtd.var(x = x_selected_classes[[m2]][,m,drop = FALSE], weights = ww, na.rm = TRUE))
      sdsd[sdsd == 0] <- 1e-20
      return(sdsd)
    }))

    priors <- sapply(weights_classes, sum)/sum(weights)
    likelihoods <- t(sapply(1:k_classes, function(m) dnorm(x = unlist(x_selected), mean = means[[m]], sd = stds[[m]])))

    priors[is.nan(priors)] <- 1e-20
    priors[is.na(priors)] <- 1e-20
    priors[is.infinite(priors)] <- 1e100-20
    priors <- priors/sum(priors)

    likelihoods[is.nan(likelihoods)] <- 1e-20
    likelihoods[is.na(likelihoods)] <- 1e-20
    likelihoods[is.infinite(likelihoods)] <- 1e100

    posteriors <- apply(cbind(priors, likelihoods), 1, prod)
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
